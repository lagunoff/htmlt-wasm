import Control.Monad.Reader
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import HtmlT.Wasm

import "this" TodoList qualified as TodoList

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)
app = wasmApp wasmMain
foreign export ccall hs_malloc :: Int -> IO (Ptr a)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()

wasmMain :: WA ()
wasmMain = do
  domBuilderId <- asks (.dom_builder_id)
  queueExp $ ElInitBuilder domBuilderId (Id "document" `Dot` "body")
  el "style" $ text TodoList.styles
  todoListStateRef <- TodoList.eval TodoList.InitAction
  TodoList.html TodoList.TodoListConfig
    { state_ref = todoListStateRef
    }
