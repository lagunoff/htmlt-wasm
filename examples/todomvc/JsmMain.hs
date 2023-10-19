module JsmMain where

import Control.Monad.Trans
import HtmlT.WebAssembly

import "this" TodoList qualified as TodoList

jsmMain :: JSM ()
jsmMain = attachToBody do
  el "style" $ text TodoList.styles
  todoListStateRef <- lift $ TodoList.eval TodoList.InitAction
  TodoList.html TodoList.TodoListConfig
    { state_ref = todoListStateRef
    }
