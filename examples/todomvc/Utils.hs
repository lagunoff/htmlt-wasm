module Utils where

import Data.ByteString (ByteString)
import HtmlT.Wasm.Types
import HtmlT.Wasm.Base
import HtmlT.Wasm.Protocol
import HtmlT.Wasm.Marshal


readLocalStorage :: FromJSVal v => ByteString -> WA (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExp $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => ByteString -> v -> WA ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSVal
  queueExp $ Call (Id "localStorage") "setItem" [Str key, stringify val]
