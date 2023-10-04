{-|
-}
module HtmlT.Wasm.Base where

import Control.Exception
import Control.Monad.State
import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Word
import Foreign.Marshal.Alloc qualified as Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Protocol

newVar :: WA VarId
newVar = reactive \e s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId (unQueueId newQueueId)
    var_storage = Set.insert newVarId s1.var_storage
    (_, s2) = installFinalizer (CustomFinalizer (freeVar newVarId)) e s1
  in
    (newVarId, s2 {var_storage})

freeVar :: VarId -> WA ()
freeVar varId = do
  modify \s -> s { var_storage = Set.delete varId s.var_storage}
  queueExp $ FreeVar varId

newCallbackEvent :: (JValue -> WA ()) -> WA CallbackId
newCallbackEvent k = reactive \e s0 ->
  let
    (queueId, s1) = nextQueueId s0
    s2 = unsafeSubscribe (EventId queueId) k e s1
  in
    (CallbackId (unQueueId queueId), s2)

installFinalizer :: FinalizerValue -> WAEnv -> WAState -> (FinalizerKey, WAState)
installFinalizer fin e s0 =
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = FinalizerCustomId finalizerId
    finalizers = Map.alter (Just . Map.insert finalizerKey fin . fromMaybe Map.empty) e.finalizer_ns s1.finalizers
  in
    (finalizerKey, s1 {finalizers})

evalExp :: Expr -> WA JValue
evalExp e = WA \_ s -> return (s, Cmd e)

queueExp :: Expr -> WA ()
queueExp e = modify \s ->
  s {evaluation_queue = e : s.evaluation_queue}

queueIfAlive :: VarId -> Expr -> WA ()
queueIfAlive varId e = modify \s ->
  let
    evaluation_queue =
      if Set.member varId s.var_storage
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

continuationsRef :: IORef [JValue -> WA Any]
continuationsRef = unsafePerformIO $ newIORef []

wasmStateRef :: IORef WAState
wasmStateRef = unsafePerformIO $ newIORef WAState
  { var_storage = Set.fromList [0, 1]
  , evaluation_queue = []
  , subscriptions = Map.empty
  , finalizers = Map.empty
  , id_supply = 0
  , transaction_queue = Map.empty
  }

wasmApp :: WA () -> Ptr Word8 -> IO (Ptr Word8)
wasmApp wasmMain p = do
  downCmd <- Binary.decode . BSL.fromStrict <$> loadByteString p
  upCmd <- handleCommand wasmMain downCmd
  storeByteString $ BSL.toStrict $ Binary.encode upCmd

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

handleCommand :: WA () -> DownCmd -> IO UpCmd
handleCommand wasmMain = \case
  Start -> do
    result <- runTillInterruption wasmEnv wasmMain
    case result of
      Left exp -> return $ Eval exp
      Right () -> return Exit
  Return jval -> do
    tipCont <- atomicModifyIORef' continuationsRef \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case tipCont of
      Nothing ->
        return $ Eval $ UncaughtException "Protocol violation: continuation is missing"
      Just c -> do
        result <- runTillInterruption wasmEnv (c jval)
        case result of
          Left exp -> return $ Eval exp
          Right _ -> return Exit
  ExecCallbackCommand arg callbackId -> do
    let
      eventId = EventId (QueueId (unCallbackId callbackId))
      wasm = unsafeTrigger eventId arg
    result <- runTillInterruption wasmEnv (dynStep wasm)
    case result of
      Left exp -> return $ Eval exp
      Right _ -> return Exit
  where
    wasmEnv = WAEnv (DomBuilder (VarId (0))) (VarId 1) (-1)

runTillInterruption :: forall a. WAEnv -> WA a -> IO (Either Expr a)
runTillInterruption e wasm = do
  s0 <- readIORef wasmStateRef
  (s1, result) <- unWA wasm e s0 `catch` \(e :: SomeException) ->
    -- UncaughtException command will not return a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s0, coerceResult (Cmd (UncaughtException (Char8.pack (show e)))))
  let
    g :: forall a. WAResult a -> IO (Either Expr a)
    g r = case r of
      Pure a -> return (Right a)
      Cmd cmd -> return (Left cmd)
      FMap f i -> fmap (fmap f) (g i)
      Interrupt cmd cont -> do
        modifyIORef' continuationsRef (unsafeCoerce cont :)
        return $ Left cmd
  writeIORef wasmStateRef s1 {evaluation_queue = []}
  eexpr <- g result
  case eexpr of
    Left e ->
      return $ Left $ RevSeq $ e:s1.evaluation_queue
    Right a
      | [] <- s1.evaluation_queue -> return $ Right a
      | otherwise -> do
        let cont (_::JValue) = return (unsafeCoerce a)
        modifyIORef' continuationsRef (cont:)
        return $ Left $ RevSeq s1.evaluation_queue
  where
    coerceResult :: forall a b. WAResult a -> WAResult b
    coerceResult = unsafeCoerce
