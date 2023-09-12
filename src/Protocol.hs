{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module Protocol where

import Data.Binary (Binary)
import Data.Binary qualified  as Binary
import Data.ByteString as BS
import Data.Int
import Data.String
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Text.Printf
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BSU
import qualified Foreign.Marshal.Alloc as Alloc

data UpCmd
  = Eval { expr :: Expr }
  | NewScope
  | FreeScope { ref :: ScopeId }
  | NewDomBuilder
  | FinalizeDomBuilder DomBuilderId
  | UncaughtException ByteString
  | Exit
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data DownCmd
  = Start
  | Return Expr
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data Expr
  = Num Int64
  | Str ByteString
  | Arr [Expr]
  | Obj [(ByteString, Expr)]
  | Dot Expr ByteString
  | Assign Expr ByteString Expr
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | Var ByteString
  | Apply Expr [Expr]
  | Call Expr ByteString [Expr]
  | Ref ScopeId VarId
  | Let ScopeId VarId Expr Expr
  | Seq Expr Expr
  | El DomBuilderId ByteString [(ByteString, Expr)]
  | Text DomBuilderId ByteString
  | PopDomBuilder DomBuilderId
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype JSFunctionName = JSFunctionName { unJSFunctionName :: ByteString }
  deriving newtype (Show, IsString, Binary)

newtype ScopeId = ScopeId { unScopeId :: Int64 }
  deriving newtype (Show, Num, Binary)

newtype VarId = VarId { unVarId :: Int64 }
  deriving newtype (Show, Num, Binary)

newtype DomBuilderId = DomBuilderId { unDomBuilderId :: Int64 }
  deriving newtype (Show, Num, Binary)

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
