import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Ptr
import HtmlT.Main.JavaScript

import "this" JsMain (jsMain)

main = jsReactorApp jsMain
