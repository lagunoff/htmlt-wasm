module TodoItem where

import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import GHC.Int
import HtmlT

data TodoItemConfig = TodoItemConfig
  { state_ref :: DynRef TodoItemState
  , is_hidden_dyn :: Dynamic Bool
  , ask_delete_item :: RJS ()
  }

data TodoItemState = TodoItemState
  { title :: Text
  , completed :: Bool
  , editing :: Maybe Text
  } deriving stock (Show, Eq)

data TodoItemAction a where
  CancelAction :: TodoItemConfig -> TodoItemAction ()
  CommitAction :: TodoItemConfig -> TodoItemAction ()
  InputAction :: TodoItemConfig -> Text -> TodoItemAction ()
  DoubleClickAction :: TodoItemConfig -> TodoItemAction ()
  CheckedAction :: TodoItemConfig -> Bool -> TodoItemAction ()
  KeydownAction :: TodoItemConfig -> Int64 -> TodoItemAction ()

eval :: TodoItemAction a -> RJS a
eval = \case
  CancelAction cfg ->
    modifyRef cfg.state_ref \s -> s{editing=Nothing}
  CommitAction cfg -> do
    state <- readRef cfg.state_ref
    case state.editing of
      Just "" ->
        cfg.ask_delete_item
      Just t ->
        modifyRef cfg.state_ref \s -> s {editing=Nothing, title = t}
      Nothing ->
        pure ()
  InputAction cfg newVal ->
    modifyRef cfg.state_ref \s -> s{editing = Just newVal}
  DoubleClickAction cfg -> do
    modifyRef cfg.state_ref \s -> s {editing = Just s.title}
--    liftIO $ js_todoItemInputFocus targetEl
  CheckedAction cfg isChecked -> do
    modifyRef cfg.state_ref \s -> s{completed = isChecked}
  KeydownAction cfg key -> case key of
    13 {- Enter -} -> eval (CommitAction cfg)
    27 {- Escape -} -> eval (CancelAction cfg)
    _ -> return ()

html :: TodoItemConfig -> Html ()
html cfg = li_ do
  let
    completedDyn = (.completed) <$> fromRef cfg.state_ref
    editingDyn = isJust . (.editing) <$> fromRef cfg.state_ref
    valueDyn = fromMaybe "" . (.editing) <$> fromRef cfg.state_ref
  toggleClass "completed" completedDyn
  toggleClass "editing" editingDyn
  toggleClass "hidden" cfg.is_hidden_dyn
  div_ [class_ "view"] do
    on @"dblclick" $ eval $ DoubleClickAction cfg
    input_ [class_ "toggle", type_ "checkbox"] do
      dynChecked $ (.completed) <$> fromRef cfg.state_ref
      on @"checkbox/change" $ eval . CheckedAction cfg
    label_ $ dynText $ (.title) <$> fromRef cfg.state_ref
    button_ [class_ "destroy"] do
      on @"click" cfg.ask_delete_item
  input_ [class_ "edit", type_ "text"] do
    dynValue valueDyn
    on @"input" $ eval . InputAction cfg
    on @"keydown" $ eval . KeydownAction cfg
    on @"blur" $ eval (CommitAction cfg)

emptyTodoItemState :: TodoItemState
emptyTodoItemState = TodoItemState "" False Nothing

instance ToJSVal TodoItemState where
  toJSVal s = Object
    [ ("title", toJSVal s.title)
    , ("completed", toJSVal s.completed)
    ]

instance FromJSVal TodoItemState where
  fromJSVal (Object kv) = do
    title <- fromJSVal =<< List.lookup "title" kv
    completed <- fromJSVal =<< List.lookup "completed" kv
    return TodoItemState {editing=Nothing, ..}
  fromJSVal _ = Nothing
