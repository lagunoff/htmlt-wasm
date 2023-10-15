import Control.Monad.Trans
import HtmlT.Wasm
import HtmlT.Wasm.DevServer

import "this" TodoList qualified as TodoList

main :: IO ()
main = runDebugDefault 8081 wasmMain

wasmMain :: JSM ()
wasmMain = attachToBody do
  el "style" $ text TodoList.styles
  todoListStateRef <- lift $ TodoList.eval TodoList.InitAction
  TodoList.html TodoList.TodoListConfig
    { state_ref = todoListStateRef
    }
