{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HtmlT.Wasm.Html where

import Data.ByteString.Char8 qualified as Char8
import Control.Monad
import Control.Monad.Reader
import Data.ByteString
import Data.IORef
import Data.Maybe
import Data.String
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Data.Map qualified as Map

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Types qualified as WAS (WASMState(..))
import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Marshal
import "this" HtmlT.Wasm.Event

el :: ByteString -> WASM a -> WASM a
el tagName child = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElPush domBuilderId tagName)
  result <- child
  queueExp (ElPop domBuilderId)
  return result

prop :: ToJSVal v => ByteString -> v -> WASM ()
prop propName propVal = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElProp domBuilderId propName (fromJValue (toJSVal propVal)))

dynProp :: ToJSVal v => ByteString -> Dynamic v -> WASM ()
dynProp propName valueDyn = do
  domBuilderVar <- newVar
  domBuilderId <- asks (.dom_builder_id)
  initialVal <- readDyn valueDyn
  queueExp (LAssign (LVar domBuilderVar) (ReadLhs (unElBuilder domBuilderId)))
  queueExp (ElProp domBuilderId propName (fromJValue (toJSVal initialVal)))
  subscribe (updates valueDyn) $
    queueExp . ElProp (ElBuilder (LVar domBuilderVar)) propName . fromJValue . toJSVal

toggleClass :: ByteString -> Dynamic Bool -> WASM ()
toggleClass className enableDyn = do
  domBuilderVar <- newVar
  domBuilderId <- asks (.dom_builder_id)
  initialVal <- readDyn enableDyn
  queueExp (LAssign (LVar domBuilderVar) (ReadLhs (unElBuilder domBuilderId)))
  queueExp (ElToggleClass domBuilderId className initialVal)
  subscribe (updates enableDyn) $
    queueExp . ElToggleClass (ElBuilder (LVar domBuilderVar)) className

attr :: ByteString -> ByteString -> WASM ()
attr attrName attrVal = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElAttr domBuilderId attrName attrVal)

on_ :: ByteString -> WASM () -> WASM ()
on_ eventName k = do
  e <- ask
  callbackId <- newCallbackEvent (local (const e) . const k)
  queueExp (ElEvent e.dom_builder_id eventName (HsCallback callbackId))

on :: forall eventName. IsEventName eventName => EventListener eventName -> WASM ()
on k = do
  e <- ask
  let eventName = Char8.pack (symbolVal (Proxy @eventName))
  callbackId <- newCallbackEvent (local (const e) . mkEventListener @eventName k)
  queueExp (ElEvent e.dom_builder_id eventName (HsCallback callbackId))

class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkEventListener :: EventListener eventName -> JValue -> WASM ()

instance IsEventName "click" where
  type EventListener "click" = WASM ()
  mkEventListener k _j = k

instance IsEventName "dblclick" where
  type EventListener "dblclick" = WASM ()
  mkEventListener k _j = k

instance IsEventName "input" where
  type EventListener "input" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k

instance IsEventName "blur" where
  type EventListener "blur" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k

text :: ByteString -> WASM ()
text contents = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElText domBuilderId contents)

dynText :: Dynamic ByteString -> WASM ()
dynText dynContent = do
  domBuilderId <- asks (.dom_builder_id)
  initialContent <- readDyn dynContent
  textNodeVar <- newVar
  queueExp (LAssign (LVar textNodeVar) (ElText domBuilderId initialContent))
  subscribe (updates dynContent) $
    queueExp . ElAssignTextContent textNodeVar

dyn :: Dynamic (WASM ()) -> WASM ()
dyn d = do
  boundary <- insertBoundary
  finalizerNs <- newNamespace
  let
    setup wasm = do
      clearBoundary boundary
      finalizeNamespace finalizerNs
      wasm
    applyBoundary e = e
      { dom_builder_id = ElBuilder (LVar boundary)
      , finalizer_ns = finalizerNs
      }
  performDyn $ fmap (local applyBoundary . setup) d

simpleList
  :: forall a. Dynamic [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> WASM ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> WASM ()
simpleList listDyn h = do
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  boundary <- insertBoundary
  let
    setup :: Int -> [a] -> [ElemEnv a] -> WASM [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        newElem <- newElemEnv x
        let wasmEnv = WASMEnv (ElBuilder (LVar newElem.ee_boundary)) newElem.ee_namespace
        local (const wasmEnv) $ h idx newElem.ee_dyn_ref
        fmap (newElem:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        writeRef r.ee_dyn_ref y
        fmap (r:) $ setup (idx + 1) ys rs
    newElemEnv :: a -> WASM (ElemEnv a)
    newElemEnv a = do
      ee_dyn_ref <- newRef a
      ee_boundary <- insertBoundary
      ee_namespace <- newNamespace
      return ElemEnv {..}
    finalizeElems :: Bool -> [ElemEnv a] -> WASM ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ destroyBoundary ee.ee_boundary
      finalizeNamespace ee.ee_namespace
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
    applyBoundary e = e
      { dom_builder_id = ElBuilder (LVar boundary)
      }
  performDyn $ fmap (local applyBoundary . updateList) listDyn
  return ()

data ElemEnv a = ElemEnv
  { ee_boundary :: VarId
  , ee_dyn_ref :: DynRef a
  , ee_namespace :: FinalizerNs
  }

consoleLog :: Expr -> WASM ()
consoleLog e = queueExp (Call (Var "console") "log" [e])

-- | Run an action before the current node is detached from the DOM
installFinalizer :: WASM () -> WASM FinalizerKey
installFinalizer fin = reactive \e s0 ->
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = FinalizerCustomId finalizerId
    finalizers = Map.alter
      (Just . Map.insert finalizerKey (CustomFinalizer fin) . fromMaybe Map.empty
      ) e.finalizer_ns s1.finalizers
  in
    (finalizerKey, s1 {WAS.finalizers})

insertBoundary :: WASM VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- newVar
  queueExp (LAssign (LVar boundary) (ReadLhs (unElBuilder domBuilderId)))
  queueExp (ElInsertBoundary (ElBuilder (LVar boundary)))
  return boundary

clearBoundary :: VarId -> WASM ()
clearBoundary boundary = queueExp (ElClearBoundary (ElBuilder (LVar boundary)))

destroyBoundary :: VarId -> WASM ()
destroyBoundary boundary = queueExp (ElDestroyBuilder (ElBuilder (LVar boundary)))

instance a ~ () => IsString (WASM a) where
  fromString = text . Char8.pack
