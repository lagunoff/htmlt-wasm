module Utils where

import HtmlT.Wasm.Types
import HtmlT.Wasm.Base
import HtmlT.Wasm.Protocol
import HtmlT.Wasm.Marshal


readLocalStorage :: FromJSVal v => Utf8 -> WA (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExp $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => Utf8 -> v -> WA ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSVal
  queueExp $ Call (Id "localStorage") "setItem" [Str key, stringify val]
