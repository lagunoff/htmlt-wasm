{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module HtmlT.Wasm.Html where

import Data.ByteString.Char8 qualified as Char8
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString
import Data.IORef
import Data.Maybe
import Data.String
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHC.Int
import Data.Map qualified as Map

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Types qualified as WAS (WASMState(..))
import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.DOM
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
  queueExp (LAssign (LVar domBuilderVar) (ReadLhs (unDomBuilder domBuilderId)))
  queueExp (ElProp domBuilderId propName (fromJValue (toJSVal initialVal)))
  subscribe (updates valueDyn) $
    queueIfAlive domBuilderVar . ElProp (DomBuilder (LVar domBuilderVar)) propName . fromJValue . toJSVal

toggleClass :: ByteString -> Dynamic Bool -> WASM ()
toggleClass className enableDyn = do
  domBuilderId <- asks (.dom_builder_id)
  initialVal <- readDyn enableDyn
  domBuilderVar <- newVar
  queueExp (LAssign (LVar domBuilderVar) (ReadLhs (unDomBuilder domBuilderId)))
  queueExp (ElToggleClass domBuilderId className initialVal)
  subscribe (updates enableDyn) $
    queueIfAlive domBuilderVar . ElToggleClass (DomBuilder (LVar domBuilderVar)) className
  return ()

attr :: ByteString -> ByteString -> WASM ()
attr attrName attrVal = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElAttr domBuilderId attrName attrVal)

on :: forall eventName. IsEventName eventName => EventListener eventName -> WASM ()
on k = do
  e <- ask
  let
    symbolStr = Char8.pack $ (symbolVal (Proxy @eventName))
    eventName = fromMaybe symbolStr . listToMaybe . List.reverse . Char8.split '/' $ symbolStr
  callbackId <- newCallbackEvent (local (const e) . mkEventListener @eventName k)
  queueExp (ElEvent e.dom_builder_id eventName (mkEventListener1 @eventName callbackId))

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
    queueIfAlive textNodeVar . ElAssignTextContent textNodeVar

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
      { dom_builder_id = DomBuilder (LVar boundary)
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
        let wasmEnv = WASMEnv (DomBuilder (LVar newElem.ee_boundary)) newElem.ee_namespace
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
      ee_namespace <- newNamespace
      local (\e -> e {finalizer_ns = ee_namespace}) do
        ee_dyn_ref <- newRef a
        ee_boundary <- insertBoundary
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
      { dom_builder_id = DomBuilder (LVar boundary)
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

insertBoundary :: WASM VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- newVar
  queueExp (LAssign (LVar boundary) (ReadLhs (unDomBuilder domBuilderId)))
  queueExp (ElInsertBoundary (DomBuilder (LVar boundary)))
  return boundary

clearBoundary :: VarId -> WASM ()
clearBoundary boundary = queueExp (ElClearBoundary (DomBuilder (LVar boundary)))

destroyBoundary :: VarId -> WASM ()
destroyBoundary boundary = queueExp (ElDestroyBuilder (DomBuilder (LVar boundary)))

instance a ~ () => IsString (WASM a) where
  fromString = text . Char8.pack
