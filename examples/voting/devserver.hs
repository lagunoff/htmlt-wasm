import HtmlT.Main.DevServer

import "this" JsmMain (jsmMain)

main :: IO ()
main = runDebugDefault 8002 $ const jsmMain
