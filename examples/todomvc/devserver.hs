import HtmlT.Wasm.DevServer

import "this" JsmMain (jsmMain)

main :: IO ()
main = runDebugDefault 8081 jsmMain
