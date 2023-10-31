module HtmlT.Marshal where

import Data.Maybe
import GHC.Int
import GHC.Generics as G
import Data.Kind
import Data.List qualified as List

import "this" HtmlT.Protocol
import "this" HtmlT.Protocol.JNumber (JNumber(..))
import "this" HtmlT.Protocol.JNumber qualified as JNumber
import "this" HtmlT.Protocol.Utf8 (Utf8(..))
import "this" HtmlT.Protocol.Utf8 qualified as Utf8

class ToJSVal a where
  toJSVal :: a -> JValue
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> JValue
  toJSVal = gToJSVal . G.from

instance ToJSVal JValue where toJSVal = Prelude.id

instance ToJSVal Bool where toJSVal = JBool

instance ToJSVal Int64 where toJSVal i = JNum (JNumber i 0)

instance ToJSVal Rational where
  toJSVal r = JNum (JNumber.jsNumberFromRational r)

instance {-# OVERLAPPABLE #-} Real a => ToJSVal a where
  toJSVal = toJSVal . toRational

instance ToJSVal Utf8 where toJSVal = JStr

instance ToJSVal a => ToJSVal [a] where toJSVal = JArr . fmap toJSVal

instance ToJSVal a => ToJSVal (Maybe a) where toJSVal = maybe JNull toJSVal

instance (ToJSVal a, ToJSVal b) => ToJSVal (a, b) where
  toJSVal (a, b) = toJSVal [toJSVal a, toJSVal b]

instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a, b, c) where
  toJSVal (a, b, c) = toJSVal [toJSVal a, toJSVal b, toJSVal c]
--------------------------------------------------------------------------------

class FromJSVal a where
  fromJSVal :: JValue -> Maybe a
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JValue -> Maybe a
  fromJSVal = fmap G.to . gFromJSVal

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
    Just (a:b:_) -> (,) <$> fromJSVal a <*> fromJSVal b
    _ -> Nothing

instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a, b, c) where
  fromJSVal j = fromJSVal j >>= \case
    Just (a:b:c:_) -> (,,) <$> fromJSVal a <*> fromJSVal b <*> fromJSVal c
    _ -> Nothing
--------------------------------------------------------------------------------

class GFromJSVal (f :: Type -> Type) where
  gFromJSVal :: JValue -> Maybe (f a)

instance GFromJSVal f => GFromJSVal (M1 m c f) where
  gFromJSVal = fmap M1 . gFromJSVal @f

instance GFromJSObject (x :*: y) => GFromJSVal (x :*: y) where
  gFromJSVal (JObj kvs) = gFromJSObject kvs
  gFromJSVal _ = Nothing

instance {-# OVERLAPPING #-} FromJSVal a => GFromJSVal (S1 s (Rec0 a)) where
  gFromJSVal = fmap (M1 . K1) . fromJSVal @a
--------------------------------------------------------------------------------

class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f x -> JValue

instance GToJSVal f => GToJSVal (M1 m c f) where
  gToJSVal (M1 f) = gToJSVal f

instance GToJSObject (x :*: y) => GToJSVal (x :*: y) where
  gToJSVal (x :*: y) = JObj $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToJSVal a) => GToJSVal (S1 s (Rec0 a)) where
  gToJSVal (M1 (K1 a)) = toJSVal a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Utf8, JValue)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSVal a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSVal a)]
    where
      key = Utf8.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Utf8, JValue)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSVal a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromJSVal
    where
      key = Utf8.pack $ selName (undefined :: M1 S s (Rec0 a) x)
