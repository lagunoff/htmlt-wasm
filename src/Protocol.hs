{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Protocol where
import Control.Concurrent
import Data.ByteString as BS
import Data.ByteString.Unsafe as BS
import Data.String
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Generics
import GHC.Prim
import System.Exit
import System.IO
import System.IO.Unsafe
import Text.Printf
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import qualified Foreign.Marshal.Alloc as Alloc

import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Int

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr


data UpCmd
  = Assign { expr :: Expr, result :: JSValueRef }
  | Eval { expr :: Expr }
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data DownCmd
  = Start
  | Completed
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data Expr
  = Num Double
  | Str ByteString
  | Dot Expr ByteString
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Var ByteString
  | Apply Expr [Expr]
  | Call Expr ByteString [Expr]
  | Ref JSValueRef
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype JSFunctionName = JSFunctionName { unJSFunctionName :: ByteString }
  deriving newtype (Show, IsString, Binary)

newtype JSValueRef = JSValueRef { unJSValueRef :: Int64 }
  deriving newtype (Show, Num, Binary)

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)

app :: Ptr Word8 -> IO (Ptr Word8)
app p = do
  downCmd <- Binary.decode . BSL.fromStrict <$> loadByteString p
  print @DownCmd downCmd
--  storeByteString (BSL.toStrict $ Binary.encode (Eval (Call (Dot (Var "console") "log") [Str "'Fuck, this is really working!'"])) )
  case downCmd of
    Start -> do
      storeByteString $ BSL.toStrict $ Binary.encode $ Eval $
        Call (Var "console") "log" [Str "It's started!"]
    _ -> do
      storeByteString $ BSL.toStrict $ Binary.encode $ Eval $
        Call (Var "document") "createElement" [Str "div"]

foreign export ccall hs_malloc :: Int -> IO (Ptr a)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

storeByteString :: ByteString -> IO (Ptr a)
storeByteString bs = do
  let len = BS.length bs
  ptr <- Alloc.callocBytes (len + 8)
  poke @Word64 ptr (fromIntegral len)
  BSU.unsafeUseAsCStringLen bs $ \(srcCString, _) ->
    copyBytes (ptr `plusPtr` 8) srcCString len
  return (castPtr ptr)

loadByteString :: Ptr a -> IO ByteString
loadByteString ptr = do
  len <- peek @Word64 (castPtr ptr)
  let contentPtr = ptr `plusPtr` 8
  BSU.unsafePackCStringFinalizer contentPtr (fromIntegral len) (Alloc.free ptr)

storeBinary :: Binary a => a -> IO (Ptr a)
storeBinary =
  storeByteString . BSL.toStrict . Binary.encode

loadBinary :: Binary a => Ptr a -> IO a
loadBinary =
  fmap (Binary.decode . BSL.fromStrict) . loadByteString

-- Function to convert a ByteString to its hexadecimal representation
byteStringToHex :: ByteString -> ByteString
byteStringToHex =
  Char8.intercalate " " . fmap (\c -> Char8.pack $ printf "%d" c) . Char8.unpack

t_01 = byteStringToHex . BSL.toStrict . Binary.encode $ Eval (Apply (Var "log") [Str "Fuck, this is really working!"])
