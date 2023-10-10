module HtmlT.Wasm.Marshal where

import Data.Maybe
import GHC.Int

import "this" HtmlT.Wasm.Protocol

class ToJSVal a where toJSVal :: a -> JValue

instance ToJSVal Bool where toJSVal = JBool

instance ToJSVal Int64 where toJSVal = JNum

instance ToJSVal Utf8 where toJSVal = JStr

instance ToJSVal a => ToJSVal [a] where toJSVal = JArr . fmap toJSVal

instance ToJSVal a => ToJSVal (Maybe a) where toJSVal = maybe JNull toJSVal
--------------------------------------------------------------------------------

class FromJSVal a where fromJSVal :: JValue -> Maybe a

instance FromJSVal Bool where
  fromJSVal = \case JBool a -> Just a; _ -> Nothing

instance FromJSVal Int64 where
  fromJSVal = \case JNum a -> Just a; _ -> Nothing

instance FromJSVal Utf8 where
  fromJSVal = \case JStr a -> Just a; _ -> Nothing

instance FromJSVal a => FromJSVal [a] where
  fromJSVal = \case
    JArr xs -> Just (mapMaybe fromJSVal xs)
    _ -> Nothing

instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal = fmap Just . fromJSVal @a
