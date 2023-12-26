import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import HtmlT.JavaScriptBackend

import "this" JsmMain (jsmMain)

main = jsReactorApp jsmMain
