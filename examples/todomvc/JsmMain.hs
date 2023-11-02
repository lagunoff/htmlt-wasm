module JsmMain where

import Data.Maybe
import HtmlT

import "this" TodoList qualified as TodoList
import "this" Utils

jsmMain :: StartFlags -> RJS ()
jsmMain _ = do
  items <- fromMaybe [] <$> readLocalStorage "todo-items"
  todoListStateRef <- TodoList.eval $ TodoList.InitAction items
  reactive $ installFinalizer $ CustomFinalizer do
    tolistState <- readRef todoListStateRef
    saveLocalStorage "todo-items" tolistState.items
  attachToBody do
    el "style" $ text TodoList.styles
    TodoList.html TodoList.TodoListConfig
      { state_ref = todoListStateRef
      }
