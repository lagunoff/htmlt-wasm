import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import HtmlT.Main.WebAssembly

import "this" JsMain (jsMain)

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)
app = reactorApp jsMain
foreign export ccall hs_malloc :: Int -> IO (Ptr Word8)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()
