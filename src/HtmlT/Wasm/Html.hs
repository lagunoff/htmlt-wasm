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
  save_current_element <- newVar
  queueExp (ElPush domBuilderId tagName)
  result <- local (\e -> e {save_current_element}) child
  queueExp (ElPop domBuilderId)
  return result

prop :: ToJSVal v => ByteString -> v -> WASM ()
prop propName propVal = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp (ElProp domBuilderId propName (fromJValue (toJSVal propVal)))

dynProp :: (ToJSVal v, Eq v) => ByteString -> Dynamic v -> WASM ()
dynProp propName (holdUniqDyn -> valueDyn) = do
  e <- ask
  initialVal <- readDyn valueDyn
  queueExp (LAssign e.save_current_element (RVar (unDomBuilder e.dom_builder_id)))
  queueExp (ElProp e.dom_builder_id propName (fromJValue (toJSVal initialVal)))
  subscribe (updates valueDyn) $
    queueIfAlive e.save_current_element . ElProp (DomBuilder e.save_current_element) propName . fromJValue . toJSVal

toggleClass :: ByteString -> Dynamic Bool -> WASM ()
toggleClass className (holdUniqDyn -> enableDyn) = do
  e <- ask
  initialVal <- readDyn enableDyn
  queueExp (LAssign e.save_current_element (RVar (unDomBuilder e.dom_builder_id)))
  queueExp (ElToggleClass e.dom_builder_id className initialVal)
  subscribe (updates enableDyn) $
    queueIfAlive e.save_current_element . ElToggleClass (DomBuilder e.save_current_element) className

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
dynText (holdUniqDyn -> dynContent) = do
  domBuilderId <- asks (.dom_builder_id)
  initialContent <- readDyn dynContent
  textNodeVar <- newVar
  queueExp (LAssign textNodeVar (ElText domBuilderId initialContent))
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
      { dom_builder_id = DomBuilder boundary
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
        let wasmEnv = WASMEnv newElem.ee_boundary newElem.ee_save_current_element newElem.ee_namespace
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
        ee_boundary <- DomBuilder <$> insertBoundary
        ee_save_current_element <- newVar
        return ElemEnv {..}
    finalizeElems :: Bool -> [ElemEnv a] -> WASM ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ destroyBoundary (unDomBuilder ee.ee_boundary)
      finalizeNamespace ee.ee_namespace
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
    applyBoundary e = e
      { dom_builder_id = DomBuilder boundary
      }
  performDyn $ fmap (local applyBoundary . updateList) listDyn
  return ()

data ElemEnv a = ElemEnv
  { ee_boundary :: DomBuilder
  , ee_save_current_element :: VarId
  , ee_dyn_ref :: DynRef a
  , ee_namespace :: FinalizerNs
  }

instance a ~ () => IsString (WASM a) where
  fromString = text . Char8.pack
