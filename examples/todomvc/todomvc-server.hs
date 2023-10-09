import Control.Monad.Reader
import HtmlT.Wasm
import HtmlT.Wasm.DevServer

import "this" TodoList qualified as TodoList

main :: IO ()
main = runDebugPort 8081 wasmMain

wasmMain :: WA ()
wasmMain = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp $ ElInitBuilder domBuilderId (Id "document" `Dot` "body")
  el "style" $ text TodoList.styles
  todoListStateRef <- TodoList.eval TodoList.InitAction
  TodoList.html TodoList.TodoListConfig
    { state_ref = todoListStateRef
    }
