module HtmlT.Wasm.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind
import GHC.TypeLits
import GHC.Int
import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as List
import Data.Maybe
import Data.Proxy

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Marshal
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Html


consoleLog :: Expr -> WA ()
consoleLog e = queueExp (Call (Id "console") "log" [e])

on :: forall eventName. IsEventName eventName => EventListener eventName -> Html ()
on k = do
  wasmEnv <- lift ask
  let
    symbolStr = Char8.pack $ (symbolVal (Proxy @eventName))
    eventName = Utf8 . fromMaybe symbolStr . listToMaybe . List.reverse . Char8.split '/' $ symbolStr
    mkExpr callbackId = AddEventListener (Arg 0 0) eventName (mkEventListener @eventName callbackId)
  callbackId <- lift $ newCallbackEvent (local (const wasmEnv) . mkCallback @eventName k)
  modify \s -> s {rev_queue = mkExpr callbackId : s.rev_queue }

class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkCallback :: EventListener eventName -> JValue -> WA ()
  mkEventListener :: CallbackId -> Expr

instance IsEventName "click" where
  type EventListener "click" = WA ()
  mkCallback k _j = k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0))

instance IsEventName "dblclick" where
  type EventListener "dblclick" = WA ()
  mkCallback k _j = k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0))

instance IsEventName "input" where
  type EventListener "input" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value"))

instance IsEventName "blur" where
  type EventListener "blur" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value"))

instance IsEventName "keydown" where
  type EventListener "keydown" = Int64 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0 `Dot` "keyCode"))

instance IsEventName "checkbox/change" where
  type EventListener "checkbox/change" = Bool -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "checked"))

instance IsEventName "select/change" where
  type EventListener "select/change" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam (ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value"))
