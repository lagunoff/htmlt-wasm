module HtmlT.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind
import Data.ByteString
import GHC.TypeLits
import GHC.Int
import GHC.Generics
import Data.ByteString.Char8 qualified as Char8
import Data.List qualified as List
import Data.Maybe
import Data.Proxy

import "this" HtmlT.Base
import "this" HtmlT.Marshal
import "this" HtmlT.Protocol
import "this" HtmlT.JSM
import "this" HtmlT.Html
import "this" HtmlT.Protocol.Utf8 (Utf8(..))


data EventListenerOptions = EventListenerOptions
  { prevent_default :: Bool
  , stop_propagation :: Bool
  } deriving stock (Generic, Show, Eq)

defaultEventListenerOptions :: EventListenerOptions
defaultEventListenerOptions = EventListenerOptions
  { prevent_default = False
  , stop_propagation = False
  }

on :: forall eventName. IsEventName eventName => EventListener eventName -> Html ()
on k = addEventListener @eventName Nothing k

onOptions
  :: forall eventName. IsEventName eventName
  => EventListenerOptions
  -> EventListener eventName
  -> Html ()
onOptions elo k = addEventListener @eventName (Just elo) k

addEventListener
  :: forall eventName. IsEventName eventName
  => Maybe EventListenerOptions
  -> EventListener eventName
  -> Html ()
addEventListener melo k = do
  wasmEnv <- lift ask
  let
    symbolStr = Char8.pack $ (symbolVal (Proxy @eventName))
    (_, eventName) = parseEventName symbolStr
    mkExpr callbackId = AddEventListener (Arg 0 0) (Utf8 eventName)
      (mkEventListener @eventName melo callbackId)
  callbackId <- lift $ newCallbackEvent (local (const wasmEnv) . mkCallback @eventName k)
  modify \s -> s {rev_queue = mkExpr callbackId : s.rev_queue}
  where
    parseEventName :: ByteString -> (Maybe ByteString, ByteString)
    parseEventName eventSpec =
      case Char8.breakEnd (=='/') eventSpec of
        (namespace, eventName)
          | Char8.null (Char8.dropEnd 1 namespace) -> (Nothing, eventName)
          | otherwise -> (Just (Char8.dropEnd 1 namespace), eventName)

class KnownSymbol eventName => IsEventName eventName where
  type EventListener eventName :: Type
  mkCallback :: EventListener eventName -> JValue -> JSM ()
  mkEventListener :: Maybe EventListenerOptions -> CallbackId -> Expr

instance IsEventName "click" where
  type EventListener "click" = JSM ()
  mkCallback k _j = k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId Null
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "dblclick" where
  type EventListener "dblclick" = JSM ()
  mkCallback k _j = k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId Null
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "submit" where
  type EventListener "submit" = JSM ()
  mkCallback k _j = k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId Null
    : applyListenerOptions (fromMaybe defaultSubmitOptions mopts)
    where
      defaultSubmitOptions = EventListenerOptions True True

instance IsEventName "input" where
  type EventListener "input" = Utf8 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "keydown" where
  type EventListener "keydown" = Int64 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "keyCode")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "keyup" where
  type EventListener "keyup" = Int64 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "keyCode")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "input/blur" where
  type EventListener "input/blur" = Utf8 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "input/focus" where
  type EventListener "input/focus" = Utf8 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "blur" where
  type EventListener "blur" = JSM ()
  mkCallback k _j = k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId Null
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "focus" where
  type EventListener "focus" = JSM ()
  mkCallback k _j = k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId Null
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "checkbox/change" where
  type EventListener "checkbox/change" = Bool -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "checked")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

instance IsEventName "select/change" where
  type EventListener "select/change" = Utf8 -> JSM ()
  mkCallback k j = forM_ (fromJSVal j) k
  mkEventListener mopts callbackId = Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions (fromMaybe defaultEventListenerOptions mopts)

applyListenerOptions :: EventListenerOptions -> [Expr]
applyListenerOptions elo = List.concat
  [ if elo.prevent_default then [Call (Arg 0 0) "preventDefault" []] else []
  , if elo.stop_propagation then [Call (Arg 0 0) "stopPropagation" []] else []
  ]

consoleLog :: Expr -> JSM ()
consoleLog e = queueExp (Call (Id "console") "log" [e])
