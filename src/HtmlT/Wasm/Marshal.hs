module HtmlT.Wasm.Marshal where

import Data.Maybe
import GHC.Int

import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Protocol.JNumber (JNumber(..))
import "this" HtmlT.Wasm.Protocol.JNumber qualified as JNumber
import "this" HtmlT.Wasm.Protocol.Utf8 (Utf8(..))

class ToJSVal a where toJSVal :: a -> JValue

instance ToJSVal JValue where toJSVal = Prelude.id

instance ToJSVal Bool where toJSVal = JBool

instance ToJSVal Int64 where toJSVal i = JNum (JNumber i 0)

instance ToJSVal Rational where
  toJSVal r = JNum (JNumber.jsNumberFromRational r)

instance {-# OVERLAPPABLE #-} Real a => ToJSVal a where toJSVal = toJSVal . toRational

instance ToJSVal Utf8 where toJSVal = JStr

instance ToJSVal a => ToJSVal [a] where toJSVal = JArr . fmap toJSVal

instance ToJSVal a => ToJSVal (Maybe a) where toJSVal = maybe JNull toJSVal

instance (ToJSVal a, ToJSVal b) => ToJSVal (a, b) where toJSVal (a, b) = toJSVal [toJSVal a, toJSVal b]
--------------------------------------------------------------------------------

class FromJSVal a where fromJSVal :: JValue -> Maybe a

instance FromJSVal JValue where fromJSVal = pure

instance FromJSVal Bool where
  fromJSVal = \case JBool a -> Just a; _ -> Nothing

instance FromJSVal Int64 where
  fromJSVal = \case
    JNum (JNumber c e)
      | e >= 0 -> Just (c * (10 ^ e))
      -- Ignoring the remainder after decimal point
      | otherwise -> Just (fst (quotRem c (10 ^ (-e))))
    _ -> Nothing

instance FromJSVal Rational where
  fromJSVal = \case
    JNum j -> Just (JNumber.jsNumberToRational j)
    _ -> Nothing

instance {-# OVERLAPPABLE #-} Fractional a => FromJSVal a where
  fromJSVal = fmap fromRational . fromJSVal

instance FromJSVal Utf8 where
  fromJSVal = \case JStr a -> Just a; _ -> Nothing

instance FromJSVal a => FromJSVal [a] where
  fromJSVal = \case
    JArr xs -> Just (mapMaybe fromJSVal xs)
    _ -> Nothing

instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal = fmap Just . fromJSVal @a

instance (FromJSVal a, FromJSVal b) => FromJSVal (a, b) where
  fromJSVal j = fromJSVal j >>= \case
    Just [a, b] -> (,) <$> fromJSVal a <*> fromJSVal b
    _ -> Nothing
