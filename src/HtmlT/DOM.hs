module HtmlT.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind
import Data.List qualified as List
import GHC.Generics
import GHC.Int

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

on :: forall eventName. IsEventName eventName => HaskellCallback eventName -> Html ()
on k = addEventListener (addEventListenerArgs @eventName) k

data AddEventListenerArgs hsCallback = AddEventListenerArgs
  { event_name :: Utf8
  , listener_options :: EventListenerOptions
  , mk_hs_callback :: hsCallback -> JValue -> JSM ()
  , mk_js_callback :: EventListenerOptions -> CallbackId -> Expr
  } deriving (Generic)

addEventListener :: forall hsCallback
  . AddEventListenerArgs hsCallback
  -> hsCallback
  -> Html ()
addEventListener args k = do
  wasmEnv <- lift ask
  let
    mkExpr callbackId = AddEventListener (Arg 0 0) args.event_name
      (args.mk_js_callback args.listener_options callbackId)
  callbackId <- lift $ newCallbackEvent (local (const wasmEnv) . args.mk_hs_callback k)
  modify \s -> s {rev_queue = mkExpr callbackId : s.rev_queue}

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
pointerEventArgs :: Utf8 -> AddEventListenerArgs (JSM ())
pointerEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ ExecCallback callbackId Null : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitEventArgs :: AddEventListenerArgs (JSM ())
submitEventArgs = AddEventListenerArgs
  { event_name = "submit"
  , listener_options = defaultSubmitOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ ExecCallback callbackId Null : applyListenerOptions opts
  }
  where
    defaultSubmitOptions = EventListenerOptions True True

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputEventArgs :: AddEventListenerArgs (Utf8 -> JSM ())
inputEventArgs = AddEventListenerArgs
  { event_name = "input"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardEventArgs :: Utf8 -> AddEventListenerArgs (Int64 -> JSM ())
keyboardEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "keyCode")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusEventArgs :: Utf8 -> AddEventListenerArgs (JSM ())
focusEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ ExecCallback callbackId Null : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeEventArgs :: AddEventListenerArgs (Bool -> JSM ())
checkboxChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "checked")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeEventArgs :: AddEventListenerArgs (Utf8 -> JSM ())
selectChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ ExecCallback callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions opts
  }

class IsEventName eventName where
  type HaskellCallback eventName :: Type
  addEventListenerArgs :: AddEventListenerArgs (HaskellCallback eventName)

instance IsEventName "click" where
  type HaskellCallback "click" = JSM ()
  addEventListenerArgs = pointerEventArgs "click"

instance IsEventName "mousedown" where
  type HaskellCallback "mousedown" = JSM ()
  addEventListenerArgs = pointerEventArgs "mousedown"

instance IsEventName "mouseup" where
  type HaskellCallback "mouseup" = JSM ()
  addEventListenerArgs = pointerEventArgs "mouseup"

instance IsEventName "dblclick" where
  type HaskellCallback "dblclick" = JSM ()
  addEventListenerArgs = pointerEventArgs "dblclick"

instance IsEventName "submit" where
  type HaskellCallback "submit" = JSM ()
  addEventListenerArgs = submitEventArgs

instance IsEventName "input" where
  type HaskellCallback "input" = Utf8 -> JSM ()
  addEventListenerArgs = inputEventArgs

instance IsEventName "keydown" where
  type HaskellCallback "keydown" = Int64 -> JSM ()
  addEventListenerArgs = keyboardEventArgs "keydown"

instance IsEventName "keyup" where
  type HaskellCallback "keyup" = Int64 -> JSM ()
  addEventListenerArgs = keyboardEventArgs "keyup"

instance IsEventName "focus" where
  type HaskellCallback "focus" = JSM ()
  addEventListenerArgs = pointerEventArgs "focus"

instance IsEventName "blur" where
  type HaskellCallback "blur" = JSM ()
  addEventListenerArgs = pointerEventArgs "blur"

instance IsEventName "input/blur" where
  type HaskellCallback "input/blur" = Utf8 -> JSM ()
  addEventListenerArgs = inputEventArgs {event_name = "blur"}

instance IsEventName "input/focus" where
  type HaskellCallback "input/focus" = Utf8 -> JSM ()
  addEventListenerArgs = inputEventArgs {event_name = "focus"}

instance IsEventName "checkbox/change" where
  type HaskellCallback "checkbox/change" = Bool -> JSM ()
  addEventListenerArgs = checkboxChangeEventArgs

instance IsEventName "select/change" where
  type HaskellCallback "select/change" = Utf8 -> JSM ()
  addEventListenerArgs = selectChangeEventArgs

applyListenerOptions :: EventListenerOptions -> [Expr]
applyListenerOptions elo = List.concat
  [ if elo.prevent_default then [Call (Arg 0 0) "preventDefault" []] else []
  , if elo.stop_propagation then [Call (Arg 0 0) "stopPropagation" []] else []
  ]

consoleLog :: Expr -> JSM ()
consoleLog e = queueExp (Call (Id "console") "log" [e])