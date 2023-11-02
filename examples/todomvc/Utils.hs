module Utils where

import HtmlT

readLocalStorage :: FromJSVal v => Utf8 -> RJS (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => Utf8 -> v -> RJS ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSVal
  enqueueExpr $ Call (Id "localStorage") "setItem" [Str key, stringify val]
