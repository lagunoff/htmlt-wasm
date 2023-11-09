module Utils where

import Data.Text (Text)

import HtmlT

readLocalStorage :: FromJSVal v => Text -> RJS (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [StringE key]
  return $ fromJSVal jsval

saveLocalStorage :: ToJSVal v => Text -> v -> RJS ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . jsvalToExpr . toJSVal
  enqueueExpr $ Call (Id "localStorage") "setItem" [StringE key, stringify val]
