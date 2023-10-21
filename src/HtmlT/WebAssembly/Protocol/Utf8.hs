module HtmlT.WebAssembly.Protocol.Utf8
  ( Utf8(..)
  , pack
  , unpack
  , utf8Show
  , empty
  , strip
  , stripPrefix
  , stripSuffix
  , breakOn
  , splitOn
  , intercalate
  , HtmlT.WebAssembly.Protocol.Utf8.drop
  , HtmlT.WebAssembly.Protocol.Utf8.take
  , encodeURIComponent
  , decodeURIComponent
  , toLower
  , toUpper
  , isInfixOf
  , HtmlT.WebAssembly.Protocol.Utf8.null
  ) where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.String (IsString(..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Char qualified as C


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

utf8Show :: Show a => a -> Utf8
utf8Show = pack . show
{-# INLINE utf8Show #-}

empty :: Utf8
empty = Utf8 BS.empty
{-# NOINLINE empty #-}

strip :: Utf8 -> Utf8
strip = Utf8 . BS.dropWhile isSpace . BS.dropWhileEnd isSpace . unUtf8
  where
    isSpace = (== 32)
{-# INLINE strip #-}

stripPrefix :: Utf8 -> Utf8 -> Maybe Utf8
stripPrefix (Utf8 str) (Utf8 prefix) =
  fmap Utf8 $ BS.stripPrefix str prefix
{-# INLINE stripPrefix #-}

stripSuffix :: Utf8 -> Utf8 -> Maybe Utf8
stripSuffix (Utf8 str) (Utf8 suffix) =
  fmap Utf8 $ BS.stripSuffix str suffix
{-# INLINE stripSuffix #-}

breakOn :: Utf8 -> Utf8 -> (Utf8, Utf8)
breakOn (Utf8 needle) (Utf8 haystack) =
  let
    (before, after) = BS.breakSubstring needle haystack
  in
    (Utf8 before, Utf8 after)
{-# INLINE breakOn #-}

splitOn :: Utf8 -> Utf8 -> [Utf8]
splitOn _          (Utf8 bs) | BS.null bs = []
splitOn (Utf8 sep) (Utf8 bs) =
  let
    (before, after) = BS.breakSubstring sep bs
  in
    if BS.null after then [Utf8 before]
      else Utf8 before : splitOn (Utf8 sep) (Utf8 (BS.drop (BS.length sep) after))
{-# NOINLINE splitOn #-}

intercalate :: Utf8 -> [Utf8] -> Utf8
intercalate (Utf8 sep) list =
  Utf8 $ BS.intercalate sep $ fmap unUtf8 list
{-# INLINE intercalate #-}

drop :: Int -> Utf8 -> Utf8
drop n (Utf8 str) = Utf8 $ BS.drop n str
{-# INLINE drop #-}

take :: Int -> Utf8 -> Utf8
take n (Utf8 str) = Utf8 $ BS.take n str
{-# INLINE take #-}

encodeURIComponent :: Utf8 -> Utf8
encodeURIComponent =
  Utf8 . Text.encodeUtf8 . Text.pack . concatMap encodeChar . Text.unpack .
    Text.decodeUtf8 . unUtf8
  where
    encodeChar c
      | C.isAlphaNum c = [c]
      | c == ' ' = "+"
      | otherwise = '%' : showHex (C.ord c) ""

decodeURIComponent :: Utf8 -> Utf8
decodeURIComponent =
  Utf8 . Text.encodeUtf8 . Text.pack  . decode . Text.unpack .
    Text.decodeUtf8 . unUtf8
  where
    decode [] = []
    decode ('%':x1:x2:xs)
      | C.isHexDigit x1 && C.isHexDigit x2 =
        C.chr (16 * digitToInt x1 + digitToInt x2) : decode xs
    decode ('+':xs) = ' ' : decode xs
    decode (x:xs) = x : decode xs

showHex :: Int -> String -> String
showHex n acc
  | n < 16 = intToDigit n : acc
  | otherwise = let (q,r) = n `divMod` 16 in showHex q (intToDigit r : acc)

digitToInt :: Char -> Int
digitToInt c
  | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
  | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = error "digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit n
  | 0 <= n && n <= 9 = toEnum (fromEnum '0' + n)
  | 10 <= n && n <= 15 = toEnum (fromEnum 'a' + n - 10)
  | otherwise = error "intToDigit: not a digit"

toLower :: Utf8 -> Utf8
toLower = Utf8 . Text.encodeUtf8 . Text.toLower . Text.decodeUtf8 . unUtf8
{-# INLINE toLower #-}

toUpper :: Utf8 -> Utf8
toUpper = Utf8 . Text.encodeUtf8 . Text.toUpper . Text.decodeUtf8 . unUtf8
{-# INLINE toUpper #-}

isInfixOf :: Utf8 -> Utf8 -> Bool
isInfixOf (Utf8 substr) (Utf8 str) = BS.isInfixOf substr str
{-# INLINE isInfixOf #-}

null :: Utf8 -> Bool
null = BS.null . unUtf8
{-# INLINE null #-}
