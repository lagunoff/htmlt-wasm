{-|
-}
module HtmlT.WASM where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
import GHC.Int
import GHC.Fingerprint
import GHC.Generics
import System.IO.Unsafe
import Unsafe.Coerce

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Event
import "this" HtmlT.Wasm.Protocol

newVar :: WASM VarId
newVar = state \old ->
  let
    newVarId = maybe 0 succ (Set.lookupMax old.storage)
    newState = old { storage = Set.insert newVarId old.storage}
  in
    (newVarId, newState)

freeVar :: VarId -> WASM ()
freeVar varId = do
  modify \old -> old { storage = Set.delete varId old.storage}
  schedExp $ FreeVar varId

withVar :: (VarId -> WASM a) -> WASM a
withVar f = do
  varId <- newVar
  result <- f varId
  result <$ freeVar varId

newCallback :: (Expr -> WASM ()) -> WASM CallbackId
newCallback k = state \old ->
  let
    id_generator = succ old.id_generator
    callbackId = CallbackId $ unQueueId old.id_generator
    callbacks = Map.insert callbackId k old.callbacks
  in
    (callbackId, old {callbacks, id_generator})

evalExp :: Expr -> WASM Expr
evalExp e = WASM \_ s -> return (s, Cmd e)

schedExp :: Expr -> WASM ()
schedExp e = WASM \_ s -> return
  (s {command_queue = e : s.command_queue}, Pure ())

continuationsRef :: IORef [Expr -> WASM Any]
continuationsRef = unsafePerformIO $ newIORef []

wasmStateRef :: IORef WASMState
wasmStateRef = unsafePerformIO $ newIORef WASMState
  { callbacks = Map.empty
  , storage = Set.singleton 0
  , command_queue = []
  , subscriptions = Map.empty
  , finalizers = Map.empty
  , id_generator = 0
  , transaction_queue = Map.empty
  }

handleCommand :: WASM () -> DownCmd -> IO UpCmd
handleCommand wasmMain = \case
  Start -> do
    result <- runTillInterruption wasmEnv wasmMain
    case result of
      Left exp -> return $ Eval exp
      Right () -> return Exit
  Return exp -> do
    tipCont <- atomicModifyIORef' continuationsRef \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case tipCont of
      Nothing ->
        return $ Eval $ UncaughtException "Protocol violation: continuation is missing"
      Just c -> do
        result <- runTillInterruption wasmEnv (c exp)
        case result of
          Left exp -> return $ Eval exp
          Right _ -> return Exit
  ExecCallback arg callbackId -> do
    wasmState <- readIORef wasmStateRef
    let callback = Map.lookup callbackId wasmState.callbacks
    case callback of
      Nothing ->
        return $ Eval $ UncaughtException "Broken callbackId"
      Just c -> do
        result <- runTillInterruption wasmEnv (dynStep (c arg))
        case result of
          Left exp -> return $ Eval exp
          Right _ -> return Exit
  where
    wasmEnv = WASMEnv (ElBuilder (LVar (VarId (0))))

runTillInterruption :: forall a. WASMEnv -> WASM a -> IO (Either Expr a)
runTillInterruption e wasm = do
  s <- readIORef wasmStateRef
  (s', result) <- unWasm wasm e s `catch` \(e :: SomeException) ->
    -- UncaughtException command will not return a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s, coerceResult (Cmd (UncaughtException (Char8.pack (show e)))))
  let
    g :: forall a. WASMResult a -> IO (Either Expr a)
    g r = case r of
      Pure a -> return (Right a)
      Cmd cmd -> return (Left cmd)
      FMap f i -> fmap (fmap f) (g i)
      Interrupt cmd cont -> do
        modifyIORef' continuationsRef (unsafeCoerce cont :)
        return $ Left cmd
  writeIORef wasmStateRef s' {command_queue = []}
  eexpr <- g result
  case eexpr of
    Left e -> return $ Left $ RevSeq $ e:s'.command_queue
    Right a
      | [] <- s'.command_queue -> return $ Right a
      | otherwise -> do
        let cont (_::Expr) = return (unsafeCoerce a)
        modifyIORef' continuationsRef (cont:)
        return $ Left $ RevSeq s'.command_queue
  where
    coerceResult :: forall a b. WASMResult a -> WASMResult b
    coerceResult = unsafeCoerce
