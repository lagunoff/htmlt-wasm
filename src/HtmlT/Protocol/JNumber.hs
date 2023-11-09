module HtmlT.Protocol.JNumber where

import Data.Binary (Binary)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Int
import Numeric (showFFloat)

newtype JNumber = JNumber {unJNumber :: ByteString}
  deriving newtype (Binary)

instance Show JNumber where
  show = (\n -> "(JNumber " <> n <> ")") . show . Char8.unpack . unJNumber

jsNumberFromInt64 :: Int64 -> JNumber
jsNumberFromInt64 = JNumber . Char8.pack . show

jsNumberFromDouble :: Double -> JNumber
jsNumberFromDouble n = JNumber . Char8.pack $ showFFloat Nothing n ""

jsNumberToInt64 :: JNumber -> Int64
jsNumberToInt64 = floor . read @Double . Char8.unpack . unJNumber

jsNumberToDouble :: JNumber -> Double
jsNumberToDouble = read . Char8.unpack . unJNumber
