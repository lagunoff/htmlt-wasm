import HtmlT.Main.DevServer

import "this" JsMain (jsMain)

main :: IO ()
main = runDebugDefault 8002 $ const jsMain
