module Utils where

import Data.ByteString (ByteString)
import HtmlT.Wasm.Types
import HtmlT.Wasm.Base
import HtmlT.Wasm.Protocol
import HtmlT.Wasm.Marshal


readLocalStorage :: FromJSVal v => ByteString -> WASM (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Var "JSON") "parse" . (:[])
  jsval <- evalExp $ jsonParse $ Call (Var "localStorage") "getItem" [Str key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => ByteString -> v -> WASM ()
saveLocalStorage key val = do
  let stringify = Call (Var "JSON") "stringify" . (:[]) . fromJValue . toJSVal
  queueExp $ Call (Var "localStorage") "setItem" [Str key, stringify val]
