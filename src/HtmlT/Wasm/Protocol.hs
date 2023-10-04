module HtmlT.Wasm.Protocol where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int
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
  | Return JValue
  | ExecCallbackCommand JValue CallbackId
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data Expr
  = Null
  | Boolean Bool
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

  | Id ByteString
  | Lam [ByteString] Expr
  | Apply Expr [Expr]
  | Call Expr ByteString [Expr]
  | RevSeq [Expr]
  -- ^ Sequence of the expressions is in reverse order! It starts
  -- evaluating from the end of the list to the beggining. Returns
  -- whatever the last expression evaluetes into (last being the
  -- expression from the tip of the list)

  | ExecCallback CallbackId Expr
  | LAssign VarId Expr
  | FreeVar VarId
  | RVar VarId
  | Ix Expr Int64

  | ElInitBuilder DomBuilder Expr
  | ElDestroyBuilder DomBuilder
  | ElPush DomBuilder ByteString
  | ElNoPush DomBuilder ByteString
  | ElProp DomBuilder ByteString Expr
  | ElAttr DomBuilder ByteString ByteString
  | ElEvent DomBuilder ByteString Expr
  | ElText DomBuilder ByteString
  | ElAssignTextContent VarId ByteString
  | ElPop DomBuilder
  | ElInsertBoundary DomBuilder
  | ElClearBoundary DomBuilder
  | ElToggleClass DomBuilder ByteString Bool

  | UncaughtException ByteString
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

data JValue
  = JNull
  | JBool Bool
  | JNum Int64
  | JStr ByteString
  | JArr [JValue]
  | JObj [(ByteString, JValue)]
  deriving stock (Generic, Show)
  deriving anyclass (Binary)

fromJValue :: JValue -> Expr
fromJValue = \case
  JNull -> Null
  JBool a -> Boolean a
  JNum a -> Num a
  JStr a -> Str a
  JArr xs -> Arr $ fmap fromJValue xs
  JObj kv -> Obj $ fmap (\(k, v) -> (k, fromJValue v)) kv

newtype VarId = VarId { unVarId :: Int64 }
  deriving newtype (Show, Num, Binary, Enum, Ord, Eq)

newtype CallbackId = CallbackId { unCallbackId :: Int64 }
  deriving newtype (Show, Num, Binary, Ord, Eq)

newtype DomBuilder = DomBuilder { unDomBuilder :: VarId }
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
