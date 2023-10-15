module HtmlT.Wasm.Base where

import Control.Exception
import Control.Monad.State
import Data.IORef
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Exts
import GHC.Generics
import Unsafe.Coerce

import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.JSM
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Protocol.Utf8 qualified as Utf8

newVar :: JSM VarId
newVar = reactive \e s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId (unQueueId newQueueId)
    var_storage = Set.insert newVarId s1.var_storage
    (_, s2) = installFinalizer (CustomFinalizer (freeVar newVarId)) e s1
  in
    (newVarId, s2 {var_storage})

freeVar :: VarId -> JSM ()
freeVar varId = do
  modify \s -> s { var_storage = Set.delete varId s.var_storage}
  queueExp $ FreeVar varId

newCallbackEvent :: (JValue -> JSM ()) -> JSM CallbackId
newCallbackEvent k = reactive \e s0 ->
  let
    (queueId, s1) = nextQueueId s0
    s2 = unsafeSubscribe (EventId queueId) k e s1
  in
    (CallbackId (unQueueId queueId), s2)

evalExp :: Expr -> JSM JValue
evalExp e = JSM \_ s -> return (s, Cmd e)

queueExp :: Expr -> JSM ()
queueExp e = modify \s ->
  s {evaluation_queue = e : s.evaluation_queue}

queueIfAlive :: VarId -> Expr -> JSM ()
queueIfAlive varId e = modify \s ->
  let
    evaluation_queue =
      if Set.member varId s.var_storage
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

data WasmInstance = WasmInstance
  { continuations_ref :: IORef [JValue -> JSM Any]
  , wasm_state_ref :: IORef JSMState
  } deriving (Generic)

runUntillInterruption :: WasmInstance -> JSMEnv -> JSM a -> IO (Either Expr a)
runUntillInterruption opt e wasm = do
  s0 <- readIORef opt.wasm_state_ref
  (s1, result) <- unJSM wasm e s0 `catch` \(e :: SomeException) ->
    -- UncaughtException command never returns a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s0, coerceResult (Cmd (UncaughtException (Utf8.pack (show e)))))
  let
    g :: forall a. JSMResult a -> IO (Either Expr a)
    g r = case r of
      Pure a -> return (Right a)
      Cmd cmd -> return (Left cmd)
      FMap f i -> fmap (fmap f) (g i)
      Interrupt cmd cont -> do
        modifyIORef' opt.continuations_ref (unsafeCoerce cont :)
        return $ Left cmd
  writeIORef opt.wasm_state_ref s1 {evaluation_queue = []}
  eexpr <- g result
  case eexpr of
    Left e ->
      return $ Left $ RevSeq $ e:s1.evaluation_queue
    Right a
      | [] <- s1.evaluation_queue -> return $ Right a
      | otherwise -> do
        let cont (_::JValue) = return (unsafeCoerce a)
        modifyIORef' opt.continuations_ref (cont:)
        return $ Left $ RevSeq s1.evaluation_queue
  where
    coerceResult :: forall a b. JSMResult a -> JSMResult b
    coerceResult = unsafeCoerce

handleCommand :: WasmInstance -> JSM () -> DownCmd -> IO UpCmd
handleCommand opt wasmMain = \case
  Start -> do
    writeIORef opt.continuations_ref []
    writeIORef opt.wasm_state_ref JSMState
      { var_storage = Set.fromList [0, 1]
      , evaluation_queue = []
      , subscriptions = Map.empty
      , finalizers = Map.empty
      , id_supply = 0
      , transaction_queue = Map.empty
      }
    result <- runUntillInterruption opt wasmEnv wasmMain
    case result of
      Left exp -> return $ EvalExpr exp
      Right () -> return Exit
  Return jval -> do
    tipCont <- atomicModifyIORef' opt.continuations_ref \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case tipCont of
      Nothing ->
        return $ EvalExpr $ UncaughtException "Protocol violation: continuation is missing"
      Just c -> do
        result <- runUntillInterruption opt wasmEnv (c jval)
        case result of
          Left exp -> return $ EvalExpr exp
          Right _ -> return Exit
  ExecCallbackCommand arg callbackId -> do
    let
      eventId = EventId (QueueId (unCallbackId callbackId))
      wasm = unsafeTrigger eventId arg
    result <- runUntillInterruption opt wasmEnv (dynStep wasm)
    case result of
      Left exp -> return $ EvalExpr exp
      Right _ -> return Exit
  where
    wasmEnv = JSMEnv (-1)
