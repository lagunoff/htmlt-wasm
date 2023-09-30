module HtmlT.Wasm.Protocol where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int
import Data.String
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

data UpCmd
  = Eval { expr :: Expr }
  | Exit
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data DownCmd
  = Start
  | Return Expr
  | ExecCallback Expr CallbackId
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data Expr
  = Null
  | Num Int64
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
  | RevSeq [Expr]
  -- ^ Sequence of the expressions is in reverse order! It starts
  -- evaluating from the end of the list to the beggining. Returns
  -- whatever the last expression evaluetes into (last being the
  -- expression from the tip of the list)

  | HsCallback CallbackId
  | LAssign LhsExpr Expr
  | FreeVar VarId
  | RVar VarId
  | Ix Expr Int64

  | ElPush ElBuilder ByteString
  | ElNoPush ElBuilder ByteString
  | ElProp ElBuilder ByteString Expr
  | ElEvent ElBuilder ByteString Expr
  | ElText ElBuilder ByteString
  | ElPop ElBuilder

  | UncaughtException ByteString

  | ReadLhs LhsExpr
  | ClearBoundary VarId
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data LhsExpr
  = LVar VarId
  | LIx LhsExpr Int64
  | LProp LhsExpr ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

newtype JSFunctionName = JSFunctionName { unJSFunctionName :: ByteString }
  deriving newtype (Show, IsString, Binary)

newtype VarId = VarId { unVarId :: Int64 }
  deriving newtype (Show, Num, Binary, Enum, Ord, Eq)

newtype CallbackId = CallbackId { unCallbackId :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)

newtype ElBuilder = ElBuilder { unElBuilder :: LhsExpr }
  deriving newtype (Show, Binary)

storeByteString :: ByteString -> IO (Ptr a)
storeByteString bs = do
  let len = BS.length bs
  dest <- Alloc.callocBytes (len + 8)
  poke @Word64 dest (fromIntegral len)
  BSU.unsafeUseAsCStringLen bs $ \(src, _) ->
    copyBytes (dest `plusPtr` 8) src len
  return (castPtr dest)

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
