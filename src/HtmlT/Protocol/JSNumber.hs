module HtmlT.Protocol.JSNumber where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Int
import Numeric (showFFloat)

newtype JSNumber = JSNumber {unJSNumber :: ByteString}
  deriving newtype (Binary)

instance Show JSNumber where
  show = (\n -> "(JSNumber " <> n <> ")") . show . Char8.unpack . unJSNumber

jsNumberFromInt64 :: Int64 -> JSNumber
jsNumberFromInt64 = JSNumber . Char8.pack . show

jsNumberFromDouble :: Double -> JSNumber
jsNumberFromDouble n = JSNumber . Char8.pack $ showFFloat Nothing n ""

jsNumberToInt64 :: JSNumber -> Int64
jsNumberToInt64 = floor . read @Double . Char8.unpack . unJSNumber

jsNumberToDouble :: JSNumber -> Double
jsNumberToDouble = read . Char8.unpack . unJSNumber
