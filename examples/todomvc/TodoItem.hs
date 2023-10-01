module TodoItem where

import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.Maybe
import HtmlT.Wasm.Types
import HtmlT.Wasm.Html
import HtmlT.Wasm.Element
import HtmlT.Wasm.Property
import HtmlT.Wasm.Base
import HtmlT.Wasm.Event
import HtmlT.Wasm.Protocol
import HtmlT.Wasm.Marshal

data TodoItemConfig = TodoItemConfig
  { state_ref :: DynRef TodoItemState
  , is_hidden_dyn :: Dynamic Bool
  , ask_delete_item :: WASM ()
  }

data TodoItemState = TodoItemState
  { title :: ByteString
  , completed :: Bool
  , editing :: Maybe ByteString
  } deriving stock (Show, Eq)

html :: TodoItemConfig -> WASM ()
html cfg = li_ do
  let
    completedDyn = (.completed) <$> fromRef cfg.state_ref
    editingDyn = isJust . (.editing) <$> fromRef cfg.state_ref
    valueDyn = fromMaybe "" . (.editing) <$> fromRef cfg.state_ref
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    -- on "dblclick" $ decodeEvent (propDecoder "target") $
    --   eval . DoubleClickAction cfg
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromRef cfg.state_ref
      -- on "change" $ decodeEvent checkedDecoder $
      --   eval . CheckedAction cfg
    label_ $ dynText $ (.title) <$> fromRef cfg.state_ref
    button_ [class_ "destroy"] do
      on_ "click" cfg.ask_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    -- on "input" $ decodeEvent valueDecoder $
    --   eval . InputAction cfg
    -- on "keydown" $ decodeEvent keyCodeDecoder $
    --   eval . KeydownAction cfg
    -- on_ "blur" $
    --   eval (CommitAction cfg)


instance ToJSVal TodoItemState where
  toJSVal s = JObj
    [ ("title", toJSVal s.title)
    , ("completed", toJSVal s.completed)
    ]

instance FromJSVal TodoItemState where
  fromJSVal (JObj kv) = do
    title <- fromJSVal =<< List.lookup "title" kv
    completed <- fromJSVal =<< List.lookup "completed" kv
    return TodoItemState {editing=Nothing, ..}
  fromJSVal _ = Nothing
