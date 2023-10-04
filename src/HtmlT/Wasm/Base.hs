{-|
-}
module HtmlT.Wasm.Base where

import Control.Exception
import Control.Monad.State
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.Maybe
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Protocol

newVar :: WASM VarId
newVar = reactive \e s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId (unQueueId newQueueId)
    var_storage = Set.insert newVarId s1.var_storage
    (_, s2) = installFinalizer (CustomFinalizer (freeVar newVarId)) e s1
  in
    (newVarId, s2 {var_storage})

freeVar :: VarId -> WASM ()
freeVar varId = do
  modify \s -> s { var_storage = Set.delete varId s.var_storage}
  queueExp $ FreeVar varId

newCallbackEvent :: (JValue -> WASM ()) -> WASM CallbackId
newCallbackEvent k = reactive \e s0 ->
  let
    (queueId, s1) = nextQueueId s0
    s2 = unsafeSubscribe (EventId queueId) k e s1
  in
    (CallbackId (unQueueId queueId), s2)

installFinalizer :: FinalizerValue -> WASMEnv -> WASMState -> (FinalizerKey, WASMState)
installFinalizer fin e s0 =
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = FinalizerCustomId finalizerId
    finalizers = Map.alter (Just . Map.insert finalizerKey fin . fromMaybe Map.empty) e.finalizer_ns s1.finalizers
  in
    (finalizerKey, s1 {finalizers})

evalExp :: Expr -> WASM JValue
evalExp e = WASM \_ s -> return (s, Cmd e)

queueExp :: Expr -> WASM ()
queueExp e = modify \s ->
  s {evaluation_queue = e : s.evaluation_queue}

queueIfAlive :: VarId -> Expr -> WASM ()
queueIfAlive varId e = modify \s ->
  let
    evaluation_queue =
      if Set.member varId s.var_storage
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

continuationsRef :: IORef [JValue -> WASM Any]
continuationsRef = unsafePerformIO $ newIORef []

wasmStateRef :: IORef WASMState
wasmStateRef = unsafePerformIO $ newIORef WASMState
  { var_storage = Set.singleton 0
  , evaluation_queue = []
  , subscriptions = Map.empty
  , finalizers = Map.empty
  , id_supply = 0
  , transaction_queue = Map.empty
  }

handleCommand :: WASM () -> DownCmd -> IO UpCmd
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
    wasmEnv = WASMEnv (DomBuilder (VarId (0))) (-1)

runTillInterruption :: forall a. WASMEnv -> WASM a -> IO (Either Expr a)
runTillInterruption e wasm = do
  s0 <- readIORef wasmStateRef
  (s1, result) <- unWasm wasm e s0 `catch` \(e :: SomeException) ->
    -- UncaughtException command will not return a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s0, coerceResult (Cmd (UncaughtException (Char8.pack (show e)))))
  let
    g :: forall a. WASMResult a -> IO (Either Expr a)
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
    coerceResult :: forall a b. WASMResult a -> WASMResult b
    coerceResult = unsafeCoerce
