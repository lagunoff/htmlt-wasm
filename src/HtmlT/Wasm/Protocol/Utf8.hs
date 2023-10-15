module HtmlT.Wasm.Protocol.Utf8 where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.String
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text


-- | A newtype over ByteString that containes utf8 encoded characters
newtype Utf8 = Utf8 { unUtf8 :: ByteString }
  deriving newtype (Show, Binary, Ord, Eq, Semigroup, Monoid)

instance IsString Utf8 where
  fromString = Utf8 . Text.encodeUtf8 . fromString
  {-# INLINE fromString #-}

pack :: String -> Utf8
pack = Utf8 . Text.encodeUtf8 . Text.pack
{-# INLINE pack #-}

unpack :: Utf8 -> String
unpack = Text.unpack . Text.decodeUtf8 . unUtf8
{-# INLINE unpack #-}
