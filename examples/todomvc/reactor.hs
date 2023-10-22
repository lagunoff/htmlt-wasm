import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import HtmlT

import "this" JsmMain (jsmMain)

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)
app = reactorApp jsmMain
foreign export ccall hs_malloc :: Int -> IO (Ptr Word8)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()
