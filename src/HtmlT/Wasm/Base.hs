module HtmlT.Wasm.Base where

import Control.Monad.Reader
import Data.ByteString

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Protocol

el :: ByteString -> Wasm a -> Wasm a
el tagName child = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElPush domBuilderId tagName)
  result <- child
  schedExp (ElPop domBuilderId)
  return result

prop :: ByteString -> ByteString -> Wasm ()
prop propName propVal = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElProp domBuilderId propName (Str propVal))
  return ()

on_ :: ByteString -> Wasm () -> Wasm ()
on_ eventName k = do
  domBuilderId <- asks (.dom_builder_id)
  callbackId <- newCallback (const k)
  schedExp (ElEvent domBuilderId eventName (HsCallbackVar callbackId))
  return ()

text :: ByteString -> Wasm ()
text contents = do
  domBuilderId <- asks (.dom_builder_id)
  schedExp (ElText domBuilderId contents)
  return ()

dyn :: Dynamic (Wasm ()) -> Wasm ()
dyn d = do
  return ()

simpleList
  :: forall a. Dynamic [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> Wasm ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Wasm ()
simpleList listDyn h = do
  return ()

consoleLog :: Expr -> Wasm ()
consoleLog e = schedExp (Call (Var "console") "log" [e])
