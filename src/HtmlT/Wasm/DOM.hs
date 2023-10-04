module HtmlT.Wasm.DOM where

import Control.Monad
import Data.ByteString
import Data.Kind
import GHC.TypeLits
import GHC.Int

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Marshal


class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkEventListener :: EventListener eventName -> JValue -> WASM ()
  mkEventListener1 :: CallbackId -> Expr

instance IsEventName "click" where
  type EventListener "click" = WASM ()
  mkEventListener k _j = k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e"))

instance IsEventName "dblclick" where
  type EventListener "dblclick" = WASM ()
  mkEventListener k _j = k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e"))

instance IsEventName "input" where
  type EventListener "input" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e" `Dot` "target" `Dot` "value"))

instance IsEventName "blur" where
  type EventListener "blur" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e" `Dot` "target" `Dot` "value"))

instance IsEventName "keydown" where
  type EventListener "keydown" = Int64 -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e" `Dot` "keyCode"))

instance IsEventName "checkbox/change" where
  type EventListener "checkbox/change" = Bool -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Var "e" `Dot` "target" `Dot` "checked"))
