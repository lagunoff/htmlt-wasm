module HtmlT.DOM where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Kind
import Data.List qualified as List
import Data.Text (Text)
import GHC.Generics
import GHC.Int

import "this" HtmlT.Html
import "this" HtmlT.Protocol
import "this" HtmlT.RJS
import "this" HtmlT.Protocol.JSVal


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
  { event_name :: Text
  , listener_options :: EventListenerOptions
  , mk_hs_callback :: hsCallback -> JSVal -> RJS ()
  , mk_js_callback :: EventListenerOptions -> CallbackId -> Expr
  } deriving (Generic)

addEventListener :: forall hsCallback
  . AddEventListenerArgs hsCallback
  -> hsCallback
  -> Html ()
addEventListener args k = do
  reactiveScope <- lift ask
  let
    mkExpr callbackId = AddEventListener (Arg 0 0) args.event_name
      (args.mk_js_callback args.listener_options callbackId)
  callbackId <- lift $ newCallbackEvent (local (const reactiveScope) . args.mk_hs_callback k)
  modify \s -> s {rev_queue = mkExpr callbackId : s.rev_queue}

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/click_event
pointerEventArgs :: Text -> AddEventListenerArgs (RJS ())
pointerEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ TriggerEvent callbackId NullE : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/submit_event
submitEventArgs :: AddEventListenerArgs (RJS ())
submitEventArgs = AddEventListenerArgs
  { event_name = "submit"
  , listener_options = defaultSubmitOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ TriggerEvent callbackId NullE : applyListenerOptions opts
  }
  where
    defaultSubmitOptions = EventListenerOptions True True

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/input_event
inputEventArgs :: AddEventListenerArgs (Text -> RJS ())
inputEventArgs = AddEventListenerArgs
  { event_name = "input"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ TriggerEvent callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keydown_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/keyup_event
keyboardEventArgs :: Text -> AddEventListenerArgs (Int64 -> RJS ())
keyboardEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ TriggerEvent callbackId (Arg 0 0 `Dot` "keyCode")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event
-- https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event
focusEventArgs :: Text -> AddEventListenerArgs (RJS ())
focusEventArgs event_name = AddEventListenerArgs
  { event_name
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k _ -> k
  , mk_js_callback = \opts callbackId ->
    Lam $ RevSeq $ TriggerEvent callbackId NullE : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
checkboxChangeEventArgs :: AddEventListenerArgs (Bool -> RJS ())
checkboxChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ TriggerEvent callbackId (Arg 0 0 `Dot` "target" `Dot` "checked")
    : applyListenerOptions opts
  }

-- https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/change_event
selectChangeEventArgs :: AddEventListenerArgs (Text -> RJS ())
selectChangeEventArgs = AddEventListenerArgs
  { event_name = "change"
  , listener_options = defaultEventListenerOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ TriggerEvent callbackId (Arg 0 0 `Dot` "target" `Dot` "value")
    : applyListenerOptions opts
  }

class IsEventName eventName where
  type HaskellCallback eventName :: Type
  addEventListenerArgs :: AddEventListenerArgs (HaskellCallback eventName)

instance IsEventName "click" where
  type HaskellCallback "click" = RJS ()
  addEventListenerArgs = pointerEventArgs "click"

instance IsEventName "mousedown" where
  type HaskellCallback "mousedown" = RJS ()
  addEventListenerArgs = pointerEventArgs "mousedown"

instance IsEventName "mouseup" where
  type HaskellCallback "mouseup" = RJS ()
  addEventListenerArgs = pointerEventArgs "mouseup"

instance IsEventName "dblclick" where
  type HaskellCallback "dblclick" = RJS ()
  addEventListenerArgs = pointerEventArgs "dblclick"

instance IsEventName "submit" where
  type HaskellCallback "submit" = RJS ()
  addEventListenerArgs = submitEventArgs

instance IsEventName "input" where
  type HaskellCallback "input" = Text -> RJS ()
  addEventListenerArgs = inputEventArgs

instance IsEventName "keydown" where
  type HaskellCallback "keydown" = Int64 -> RJS ()
  addEventListenerArgs = keyboardEventArgs "keydown"

instance IsEventName "keyup" where
  type HaskellCallback "keyup" = Int64 -> RJS ()
  addEventListenerArgs = keyboardEventArgs "keyup"

instance IsEventName "focus" where
  type HaskellCallback "focus" = RJS ()
  addEventListenerArgs = pointerEventArgs "focus"

instance IsEventName "blur" where
  type HaskellCallback "blur" = RJS ()
  addEventListenerArgs = pointerEventArgs "blur"

instance IsEventName "input/blur" where
  type HaskellCallback "input/blur" = Text -> RJS ()
  addEventListenerArgs = inputEventArgs {event_name = "blur"}

instance IsEventName "input/focus" where
  type HaskellCallback "input/focus" = Text -> RJS ()
  addEventListenerArgs = inputEventArgs {event_name = "focus"}

instance IsEventName "checkbox/change" where
  type HaskellCallback "checkbox/change" = Bool -> RJS ()
  addEventListenerArgs = checkboxChangeEventArgs

instance IsEventName "select/change" where
  type HaskellCallback "select/change" = Text -> RJS ()
  addEventListenerArgs = selectChangeEventArgs

-- | Collection of deltaX, deltaY and deltaZ properties from WheelEvent
-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
data MouseWheel = MouseWheel
  { mw_delta_x :: Int64
  , mw_delta_y :: Int64
  , mw_delta_z :: Int64
  , mw_alt_key :: Bool
  , mw_ctrl_key :: Bool
  , mw_meta_key :: Bool
  , mw_shift_key :: Bool
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSVal, ToJSVal)

-- https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent
mouseWheelEventArgs :: AddEventListenerArgs (MouseWheel -> RJS ())
mouseWheelEventArgs = AddEventListenerArgs
  { event_name = "mousewheel"
  , listener_options = defaultMouseWheelOptions
  , mk_hs_callback = \k j -> forM_ (fromJSVal j) k
  , mk_js_callback = \opts callbackId -> Lam $ RevSeq
    $ TriggerEvent callbackId (ObjectE
      [ ("mw_delta_x", Arg 0 0 `Dot` "deltaX")
      , ("mw_delta_y", Arg 0 0 `Dot` "deltaY")
      , ("mw_delta_z", Arg 0 0 `Dot` "deltaZ")
      , ("mw_alt_key", Arg 0 0 `Dot` "altKey")
      , ("mw_ctrl_key", Arg 0 0 `Dot` "ctrlKey")
      , ("mw_meta_key", Arg 0 0 `Dot` "metaKey")
      , ("mw_shift_key", Arg 0 0 `Dot` "shiftKey")
      ])
    : applyListenerOptions opts
  }
  where
    defaultMouseWheelOptions = EventListenerOptions True True

instance IsEventName "mousewheel" where
  type HaskellCallback "mousewheel" = MouseWheel -> RJS ()
  addEventListenerArgs = mouseWheelEventArgs

applyListenerOptions :: EventListenerOptions -> [Expr]
applyListenerOptions elo = List.concat
  [ if elo.prevent_default then [Call (Arg 0 0) "preventDefault" []] else []
  , if elo.stop_propagation then [Call (Arg 0 0) "stopPropagation" []] else []
  ]

consoleLog :: Expr -> RJS ()
consoleLog e = enqueueExpr (Call (Id "console") "log" [e])
