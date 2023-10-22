import HtmlT.DevServer

import "this" JsmMain (jsmMain)

main :: IO ()
main = runDebugDefault 8002 jsmMain
