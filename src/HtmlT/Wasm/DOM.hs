module HtmlT.Wasm.DOM where

import Control.Monad
import Control.Monad.Reader
import Data.ByteString
import Data.Kind
import GHC.TypeLits
import GHC.Int

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Marshal


consoleLog :: Expr -> WASM ()
consoleLog e = queueExp (Call (Id "console") "log" [e])

insertBoundary :: WASM VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- newVar
  queueExp (LAssign boundary (RVar (unDomBuilder domBuilderId)))
  queueExp (ElInsertBoundary (DomBuilder boundary))
  return boundary

clearBoundary :: VarId -> WASM ()
clearBoundary boundary = queueExp (ElClearBoundary (DomBuilder boundary))

destroyBoundary :: VarId -> WASM ()
destroyBoundary boundary = queueExp (ElDestroyBuilder (DomBuilder boundary))

class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkEventListener :: EventListener eventName -> JValue -> WASM ()
  mkEventListener1 :: CallbackId -> Expr

instance IsEventName "click" where
  type EventListener "click" = WASM ()
  mkEventListener k _j = k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e"))

instance IsEventName "dblclick" where
  type EventListener "dblclick" = WASM ()
  mkEventListener k _j = k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e"))

instance IsEventName "input" where
  type EventListener "input" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "value"))

instance IsEventName "blur" where
  type EventListener "blur" = ByteString -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "value"))

instance IsEventName "keydown" where
  type EventListener "keydown" = Int64 -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "keyCode"))

instance IsEventName "checkbox/change" where
  type EventListener "checkbox/change" = Bool -> WASM ()
  mkEventListener k j = forM_ (fromJSVal j) k
  mkEventListener1 callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "checked"))
