{-# LANGUAGE NondecreasingIndentation #-}
module TodoList where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe
import HtmlT.Wasm.Types
import HtmlT.Wasm.Base
import HtmlT.Wasm.Element
import HtmlT.Wasm.Html
import HtmlT.Wasm.Property
import HtmlT.Wasm.Event
import HtmlT.Wasm.Protocol
import HtmlT.Wasm.Marshal

import "this" Utils
import "this" TodoItem qualified as TodoItem


data TodoListConfig = TodoListConfig
  { state_ref :: DynRef TodoListState
  }

data TodoListState = TodoListState
  { title :: ByteString
  , items :: [TodoItem.TodoItemState]
  , filter :: Filter
  } deriving (Show, Eq)

data Filter = All | Active | Completed
  deriving (Show, Eq)


init :: WASM (DynRef TodoListState)
init = do
  items <- fromMaybe [] <$> readLocalStorage "todo-items"
  newRef TodoListState
    { title = ""
    , items = items
    , filter = All
    }

html :: TodoListConfig -> WASM ()
html cfg = do
  div_ do
    section_ [class_ "todoapp"] do
      headerWidget
      mainWidget
      footerWidget
    footerInfoWidget
  where
    headerWidget = header_ [class_ "header"] do
      h1_ (text "todos")
      input_ [class_ "new-todo", placeholder_ "What needs to be done?", autofocus_ True] do
        dynValue $ (.title) <$> fromRef cfg.state_ref
        -- on "input" $ decodeEvent valueDecoder $
        --   eval . InputAction cfg
        -- on "keydown" $ decodeEvent keyCodeDecoder $
        --   eval . KeydownAction cfg
    mainWidget = section_ [class_ "main"] do
      toggleClass "hidden" hiddenDyn
      input_ [id_ "toggle-all", class_ "toggle-all", type_ "checkbox"] do
        return ()
        -- on "click" $ decodeEvent checkedDecoder $
        --   eval . ToggleAllAction cfg
      label_ do
        attr "for" "toggle-all"
        text "Mark all as completed"
      ul_ [class_ "todo-list"] do
        simpleList itemsDyn \idx todoRef ->
          TodoItem.html $ TodoItem.TodoItemConfig
            { TodoItem.state_ref = todoRef
              { dynref_modifier = todoItemModifier cfg idx todoRef.dynref_modifier
              }
            , TodoItem.is_hidden_dyn =
              isTodoItemHidden <$> fromRef cfg.state_ref <*> fromRef todoRef
            , TodoItem.ask_delete_item = return () -- eval (DeleteItemAction cfg idx)
            }
    footerWidget = footer_ [class_ "footer"] do
      toggleClass "hidden" hiddenDyn
      span_ [class_ "todo-count"] do
        strong_ $ dynText $ Char8.pack . show <$> itemsLeftDyn
        dynText $ pluralize " item left" " items left" <$> itemsLeftDyn
      ul_ [class_ "filters"] do
        forM_ [All, Active, Completed] filterWidget
      button_ [class_ "clear-completed"] do
        -- on_ "click" $ eval (ClearCompletedAction cfg)
        text "Clear completed"
    footerInfoWidget = footer_ [class_ "info"] do
      p_ "Double-click to edit a todo"
      p_ do
        text "Created by "
        a_ [href_ "https://github.com/lagunoff"] "Vlad Lagunov"
      p_ do
        text "Part of "
        a_ [href_ "http://todomvc.com"] "TodoMVC"
    filterWidget :: Filter -> WASM ()
    filterWidget flt = li_ do
      a_ [href_ (printFilter flt)] do
        toggleClass "selected" $ filterSelectedDyn flt
        text $ Char8.pack (show flt)
    hiddenDyn =
      Prelude.null . (.items) <$> fromRef cfg.state_ref
    itemsLeftDyn =
      countItemsLeft <$> fromRef cfg.state_ref
    filterSelectedDyn flt =
      (==flt) . (.filter) <$> fromRef cfg.state_ref
    itemsDyn =
      (.items) <$> fromRef cfg.state_ref
    countItemsLeft TodoListState{items} =
      foldl (\acc TodoItem.TodoItemState{completed} ->
        if not completed then acc + 1 else acc) 0 items
    isTodoItemHidden :: TodoListState -> TodoItem.TodoItemState -> Bool
    isTodoItemHidden listState itemState =
      case (listState.filter, itemState.completed) of
        (Active,    True)  -> True
        (Completed, False) -> True
        _                  -> False

pluralize :: ByteString -> ByteString -> Int -> ByteString
pluralize singular _plural 0 = singular
pluralize _singular plural _ = plural

parseFilter :: ByteString -> Maybe Filter
parseFilter =  \case
  "#/"          -> Just All
  "#/active"    -> Just Active
  "#/completed" -> Just Completed
  _             -> Nothing

printFilter :: Filter -> ByteString
printFilter =  \case
  All       -> "#/"
  Active    -> "#/active"
  Completed -> "#/completed"

-- | Synchronize changes inside TodoItem widget with the outer
-- TodoList widget.
todoItemModifier
  :: TodoListConfig
  -> Int
  -> Modifier TodoItem.TodoItemState
  -> Modifier TodoItem.TodoItemState
todoItemModifier cfg idx elemModifier = Modifier \upd f -> do
  -- Update the local TodoItem element widget
  ((old, new), result) <- unModifier elemModifier upd \old ->
    let (new, result) = f old in (new, ((old, new), result))
  let
    -- When False, the update event won't be propagated into the outer
    -- widget for the sake of optimization
    needsUpdate = upd && (old.completed /= new.completed)
  -- Update the outer widget
  unModifier (dynref_modifier cfg.state_ref) needsUpdate \old ->
    (old {items = overIx idx (const new) old.items}, ())
  return result
  where
    overIx :: Int -> (a -> a) -> [a] -> [a]
    overIx 0 f (x:xs) = f x : xs
    overIx n f (x:xs) = x : overIx (pred n) f xs
    overIx n _ [] = []
