module HtmlT.Wasm.DOM where

import Control.Monad
import Control.Monad.Reader
import Data.Kind
import GHC.TypeLits
import GHC.Int

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Marshal
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Types


insertBoundary :: WA VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- newVar
  queueExp (AssignVar boundary (Var (unDomBuilder domBuilderId)))
  queueExp (ElInsertBoundary (DomBuilder boundary))
  return boundary

clearBoundary :: VarId -> WA ()
clearBoundary boundary = queueExp (ElClearBoundary (DomBuilder boundary))

destroyBoundary :: VarId -> WA ()
destroyBoundary boundary = queueExp (ElDestroyBuilder (DomBuilder boundary))

consoleLog :: Expr -> WA ()
consoleLog e = queueExp (Call (Id "console") "log" [e])

class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkCallback :: EventListener eventName -> JValue -> WA ()
  mkEventListener :: CallbackId -> Expr

instance IsEventName "click" where
  type EventListener "click" = WA ()
  mkCallback k _j = k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e"))

instance IsEventName "dblclick" where
  type EventListener "dblclick" = WA ()
  mkCallback k _j = k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e"))

instance IsEventName "input" where
  type EventListener "input" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "value"))

instance IsEventName "blur" where
  type EventListener "blur" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "value"))

instance IsEventName "keydown" where
  type EventListener "keydown" = Int64 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "keyCode"))

instance IsEventName "checkbox/change" where
  type EventListener "checkbox/change" = Bool -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "checked"))

instance IsEventName "select/change" where
  type EventListener "select/change" = Utf8 -> WA ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener callbackId = Lam ["e"] (ExecCallback callbackId (Id "e" `Dot` "target" `Dot` "value"))
