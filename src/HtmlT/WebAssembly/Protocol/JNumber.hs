module HtmlT.WebAssembly.Protocol.JNumber where

import Control.Exception (throw, ArithException(DivideByZero))
import Data.Binary (Binary)
import Data.Int
import GHC.Generics
import Data.Ratio

-- | Copy-pasted from @scientific@ package
data JNumber = JNumber
  { coefficient :: {-# UNPACK #-} Int64
  , base10Exponent :: {-# UNPACK #-} Int8
  } deriving stock (Generic, Show)
    deriving anyclass (Binary)

jsNumberToRational :: JNumber -> Rational
jsNumberToRational (JNumber c e)
  | e < 0 =  fromIntegral c % magnitude (-e)
  | otherwise = fromIntegral (c * magnitude   e) % 1
  where
    magnitude e = 10 ^ e
{-# INLINABLE jsNumberToRational #-}

jsNumberFromRational :: Rational -> JNumber
jsNumberFromRational rational
  | d == 0 = throw DivideByZero
  | otherwise = positivize (longDiv 0 0) (fromIntegral (numerator rational))
  where
    -- Divide the numerator by the denominator using long division.
    longDiv :: Int64 -> Int8 -> (Int64 -> JNumber)
    longDiv !c !e  0 = JNumber c e
    longDiv !c !e !n
      | abs c >= maxCoefficient = JNumber c e
      | n < d     = longDiv (c * 10) (e - 1) (n * 10)
      | otherwise = case n `quotRem` d of
                      ( q, r ) -> longDiv (c + q) e r
    d = fromIntegral (denominator rational)
    positivize :: (Int64 -> JNumber) -> (Int64 -> JNumber)
    positivize f x | x < 0 = negateJNumber (f (-x))
                   | otherwise = f   x
    {-# INLINE positivize #-}
    maxCoefficient = 922337203685477580

normalize :: JNumber -> JNumber
normalize (JNumber c e)
  | c > 0 = normalizePositive c  e
  | c < 0 = negateJNumber (normalizePositive (-c) e)
  | otherwise {- c == 0 -} = JNumber 0 0
  where
    normalizePositive :: Int64 -> Int8 -> JNumber
    normalizePositive !c !e = case quotRem c 10 of
      (c', r)
        | r == 0 -> normalizePositive c' (e+1)
        | otherwise -> JNumber c e

negateJNumber :: JNumber -> JNumber
negateJNumber (JNumber c e) = JNumber (negate c) e
