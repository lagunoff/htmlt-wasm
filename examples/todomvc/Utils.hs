module Utils where

import HtmlT
import HtmlT.JSON

readLocalStorage :: FromJSON v => Utf8 -> RJS (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ fromJSON jsval

saveLocalStorage :: ToJSON v => Utf8 -> v -> RJS ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSON
  enqueueExpr $ Call (Id "localStorage") "setItem" [Str key, stringify val]
