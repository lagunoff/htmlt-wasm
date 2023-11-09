module HtmlT.JSON.Types where

import Data.Kind
import Data.List qualified as List
import Data.Maybe
import GHC.Generics as G
import GHC.Int
import Data.Binary
import Data.Text (Text)
import Data.Text qualified as Text

import "this" HtmlT.Protocol.JNumber (JNumber(..))
import "this" HtmlT.Protocol.JNumber qualified as JNumber

data Value
  = Object [(Text, Value)]
  | Array [Value]
  | String Text
  | Number JNumber
  | Bool Bool
  | Null
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = gToJSON . G.from

instance ToJSON Value where toJSON = Prelude.id

instance ToJSON Bool where toJSON = Bool

instance ToJSON Int64 where toJSON i = Number (JNumber i 0)

instance ToJSON Rational where
  toJSON r = Number (JNumber.jsNumberFromRational r)

instance {-# OVERLAPPABLE #-} Real a => ToJSON a where
  toJSON = toJSON . toRational

instance ToJSON Text where toJSON = String

instance ToJSON a => ToJSON [a] where toJSON = Array . fmap toJSON

instance ToJSON a => ToJSON (Maybe a) where toJSON = maybe Null toJSON

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
  toJSON (a, b) = toJSON [toJSON a, toJSON b]

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
  toJSON (a, b, c) = toJSON [toJSON a, toJSON b, toJSON c]
--------------------------------------------------------------------------------

class FromJSON a where
  fromJSON :: Value -> Maybe a
  default fromJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Maybe a
  fromJSON = fmap G.to . gFromJSON

instance FromJSON Value where fromJSON = pure

instance FromJSON Bool where
  fromJSON = \case Bool a -> Just a; _ -> Nothing

instance FromJSON Int64 where
  fromJSON = \case
    Number (JNumber c e)
      | e >= 0 -> Just (c * (10 ^ e))
      -- Ignoring the remainder after decimal point
      | otherwise -> Just (fst (quotRem c (10 ^ (-e))))
    _ -> Nothing

instance FromJSON Rational where
  fromJSON = \case
    Number j -> Just (JNumber.jsNumberToRational j)
    _ -> Nothing

instance {-# OVERLAPPABLE #-} Fractional a => FromJSON a where
  fromJSON = fmap fromRational . fromJSON

instance FromJSON Text where
  fromJSON = \case String a -> Just a; _ -> Nothing

instance FromJSON a => FromJSON [a] where
  fromJSON = \case
    Array xs -> Just (mapMaybe fromJSON xs)
    _ -> Nothing

instance FromJSON a => FromJSON (Maybe a) where
  fromJSON = fmap Just . fromJSON @a

instance (FromJSON a, FromJSON b) => FromJSON (a, b) where
  fromJSON j = fromJSON j >>= \case
    Just (a:b:_) -> (,) <$> fromJSON a <*> fromJSON b
    _ -> Nothing

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a, b, c) where
  fromJSON j = fromJSON j >>= \case
    Just (a:b:c:_) -> (,,) <$> fromJSON a <*> fromJSON b <*> fromJSON c
    _ -> Nothing
--------------------------------------------------------------------------------

class GFromJSON (f :: Type -> Type) where
  gFromJSON :: Value -> Maybe (f a)

instance GFromJSON f => GFromJSON (M1 m c f) where
  gFromJSON = fmap M1 . gFromJSON @f

instance GFromJSObject (x :*: y) => GFromJSON (x :*: y) where
  gFromJSON (Object kvs) = gFromJSObject kvs
  gFromJSON _ = Nothing

instance {-# OVERLAPPING #-} FromJSON a => GFromJSON (S1 s (Rec0 a)) where
  gFromJSON = fmap (M1 . K1) . fromJSON @a
--------------------------------------------------------------------------------

class GToJSON (f :: Type -> Type) where
  gToJSON :: f x -> Value

instance GToJSON f => GToJSON (M1 m c f) where
  gToJSON (M1 f) = gToJSON f

instance GToJSObject (x :*: y) => GToJSON (x :*: y) where
  gToJSON (x :*: y) = Object $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToJSON a) => GToJSON (S1 s (Rec0 a)) where
  gToJSON (M1 (K1 a)) = toJSON a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, Value)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSON a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSON a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, Value)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSON a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromJSON
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
