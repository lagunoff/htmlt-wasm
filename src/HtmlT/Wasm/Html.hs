module HtmlT.Wasm.Html where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Types qualified as WAS (WASMState(..))
import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Event

el :: ByteString -> WASM a -> WASM a
el tagName child = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElPush domBuilderId tagName)
  result <- child
  schedExp (ElPop domBuilderId)
  return result

prop :: ByteString -> ByteString -> WASM ()
prop propName propVal = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElProp domBuilderId propName (Str propVal))

on_ :: ByteString -> WASM () -> WASM ()
on_ eventName k = do
  domBuilderId <- asks (.dom_builder_id)
  callbackId <- newCallbackEvent (const k)
  schedExp (ElEvent domBuilderId eventName (HsCallback callbackId))

text :: ByteString -> WASM ()
text contents = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElText domBuilderId contents)

dyn :: Dynamic (WASM ()) -> WASM ()
dyn d = do
  boundary <- insertBoundary
  finalizerKey <- FinalizerCustomId <$> state nextQueueId
  let
    finalizeNested = do
      nested <- state removeNestedFinalizers
      case nested of
        Just (NestedFinalizer n) -> applyFinalizer n
        _ -> return ()
    setup wasm = do
      finalizeNested
      clearBoundary boundary
      withNestedFinalizers finalizerKey wasm
    applyBoundary e = e
      {dom_builder_id = ElBuilder (LVar boundary)}
    removeNestedFinalizers s =
      let
        (nested, finalizers) = Map.alterF (, Nothing) finalizerKey s.finalizers
      in
        (nested, s {WAS.finalizers})
  performDyn $ fmap (local applyBoundary . setup) d

-- simpleList
--   :: forall a. Dynamic [a]
--   -- ^ Some dynamic data from the above scope
--   -> (Int -> DynRef a -> WASM ())
--   -- ^ Function to build children widget. Accepts the index inside the
--   -- collection and dynamic data for that particular element
--   -> WASM ()
-- simpleList listDyn h = do
--   return ()

consoleLog :: Expr -> WASM ()
consoleLog e = schedExp (Call (Var "console") "log" [e])

-- | Run an action before the current node is detached from the DOM
installFinalizer :: WASM () -> WASM FinalizerKey
installFinalizer fin = state \s0 ->
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = FinalizerCustomId finalizerId
    finalizers = Map.insert finalizerKey (CustomFinalizer fin) s1.finalizers
  in
    (finalizerKey, s1 {WAS.finalizers})

insertBoundary :: WASM VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- newVar
  schedExp (LAssign (LVar boundary) (ReadLhs (unElBuilder domBuilderId)))
  return boundary

clearBoundary :: VarId -> WASM ()
clearBoundary boundary = schedExp (ClearBoundary boundary)

withNestedFinalizers :: FinalizerKey -> WASM a -> WASM a
withNestedFinalizers finkey wasm = do
  temp <- state \s ->
    let
      finalizers = extractNested $ Map.lookup finkey s.finalizers
    in
      (s.finalizers, s {WAS.finalizers})
  result <- wasm
  state \s ->
    let
      finalizers = Map.insert finkey (NestedFinalizer s.finalizers) temp
    in
      (result, s {WAS.finalizers})
  where
    extractNested (Just (NestedFinalizer n)) = n
    extractNested _ = Map.empty
