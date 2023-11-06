module Utils where

import Data.Text (Text)

import HtmlT
import HtmlT.JSON

readLocalStorage :: FromJSON v => Text -> RJS (Maybe v)
readLocalStorage key = do
  let jsonParse = Call (Id "JSON") "parse" . (:[])
  jsval <- evalExpr $ jsonParse $ Call (Id "localStorage") "getItem" [Str key]
  return $ parseMaybe parseJSON jsval

saveLocalStorage :: ToJSON v => Text -> v -> RJS ()
saveLocalStorage key val = do
  let stringify = Call (Id "JSON") "stringify" . (:[]) . fromJValue . toJSON
  enqueueExpr $ Call (Id "localStorage") "setItem" [Str key, stringify val]
