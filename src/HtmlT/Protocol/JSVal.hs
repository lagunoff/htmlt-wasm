module HtmlT.Protocol.JSVal where

import Data.Binary
import Data.Kind
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics as G
import GHC.Int

import "this" HtmlT.Protocol.JSNumber (JSNumber(..))
import "this" HtmlT.Protocol.JSNumber qualified as JSNumber

data JSVal
  = Object [(Text, JSVal)]
  | Array [JSVal]
  | String Text
  | Number JSNumber
  | Bool Bool
  | Null
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

class ToJSVal a where
  toJSVal :: a -> JSVal
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> JSVal
  toJSVal = gToJSVal . G.from

instance ToJSVal JSVal where toJSVal = Prelude.id

instance ToJSVal Bool where toJSVal = Bool

instance ToJSVal Int64 where
  toJSVal i = Number (JSNumber.jsNumberFromInt64 i)

instance ToJSVal Double where
  toJSVal d = Number (JSNumber.jsNumberFromDouble d)

instance ToJSVal Text where toJSVal = String

instance ToJSVal a => ToJSVal [a] where toJSVal = Array . fmap toJSVal

instance ToJSVal a => ToJSVal (Maybe a) where toJSVal = maybe Null toJSVal

instance (ToJSVal a, ToJSVal b) => ToJSVal (a, b) where
  toJSVal (a, b) = toJSVal [toJSVal a, toJSVal b]

instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a, b, c) where
  toJSVal (a, b, c) = toJSVal [toJSVal a, toJSVal b, toJSVal c]
--------------------------------------------------------------------------------

class FromJSVal a where
  fromJSVal :: JSVal -> Maybe a
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JSVal -> Maybe a
  fromJSVal = fmap G.to . gFromJSVal

instance FromJSVal JSVal where fromJSVal = pure

instance FromJSVal Bool where
  fromJSVal = \case Bool a -> Just a; _ -> Nothing

instance FromJSVal Int64 where
  fromJSVal = \case
    Number j -> Just (JSNumber.jsNumberToInt64 j)
    _ -> Nothing

instance FromJSVal Double where
  fromJSVal = \case
    Number j -> Just (JSNumber.jsNumberToDouble j)
    _ -> Nothing

instance FromJSVal Text where
  fromJSVal = \case String a -> Just a; _ -> Nothing

instance FromJSVal a => FromJSVal [a] where
  fromJSVal = \case
    Array xs -> Just (mapMaybe fromJSVal xs)
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
  gFromJSVal :: JSVal -> Maybe (f a)

instance GFromJSVal f => GFromJSVal (M1 m c f) where
  gFromJSVal = fmap M1 . gFromJSVal @f

instance GFromJSObject (x :*: y) => GFromJSVal (x :*: y) where
  gFromJSVal (Object kvs) = gFromJSObject kvs
  gFromJSVal _ = Nothing

instance {-# OVERLAPPING #-} FromJSVal a => GFromJSVal (S1 s (Rec0 a)) where
  gFromJSVal = fmap (M1 . K1) . fromJSVal @a
--------------------------------------------------------------------------------

class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f x -> JSVal

instance GToJSVal f => GToJSVal (M1 m c f) where
  gToJSVal (M1 f) = gToJSVal f

instance GToJSObject (x :*: y) => GToJSVal (x :*: y) where
  gToJSVal (x :*: y) = Object $ gToJSObject (x :*: y)

instance {-# OVERLAPPING #-} (ToJSVal a) => GToJSVal (S1 s (Rec0 a)) where
  gToJSVal (M1 (K1 a)) = toJSVal a
--------------------------------------------------------------------------------

class GToJSObject (f :: Type -> Type) where
  gToJSObject :: f x -> [(Text, JSVal)]

instance (GToJSObject x, GToJSObject y) => GToJSObject (x :*: y) where
  gToJSObject (x :*: y) = gToJSObject x <> gToJSObject y

instance (GToJSObject f) => GToJSObject (M1 m c f) where
  gToJSObject (M1 a) = gToJSObject a

instance {-# OVERLAPPING #-} (ToJSVal a, Selector s) => GToJSObject (S1 s (Rec0 a)) where
  gToJSObject (M1 (K1 a)) = [(key, toJSVal a)]
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
--------------------------------------------------------------------------------

class GFromJSObject (f :: Type -> Type) where
  gFromJSObject :: [(Text, JSVal)] -> Maybe (f x)

instance (GFromJSObject x, GFromJSObject y) => GFromJSObject (x :*: y) where
  gFromJSObject kvs = liftA2 (:*:) (gFromJSObject kvs) (gFromJSObject kvs)

instance (GFromJSObject f) => GFromJSObject (M1 m c f) where
  gFromJSObject = fmap M1 . gFromJSObject

instance {-# OVERLAPPING #-} (FromJSVal a, Selector s) => GFromJSObject (S1 s (Rec0 a)) where
  gFromJSObject kvs = List.lookup key kvs >>= fmap (M1 . K1) . fromJSVal
    where
      key = Text.pack $ selName (undefined :: M1 S s (Rec0 a) x)
