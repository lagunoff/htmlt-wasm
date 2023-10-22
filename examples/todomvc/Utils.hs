module Utils where

import HtmlT

readLocalStorage :: FromJSVal v => Utf8 -> JSM (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExp $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => Utf8 -> v -> JSM ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSVal
  queueExp $ Call (Id "localStorage") "setItem" [Str key, stringify val]
