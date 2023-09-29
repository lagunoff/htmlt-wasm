{-|
-}
module HtmlT.Wasm.Types where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

import "this" HtmlT.Wasm.Protocol

data WasmResult a where
  Pure :: a -> WasmResult a
  Cmd :: Expr -> WasmResult Expr
  Interrupt :: Expr -> (Expr -> Wasm b) -> WasmResult b
  FMap :: (a -> b) -> WasmResult a -> WasmResult b

-- | A computation capable of interacting with JavaScript.
newtype Wasm a = Wasm
  { unWasm :: WasmEnv -> WasmState -> IO (WasmState, WasmResult a)
  }

data WasmEnv = WasmEnv
  { dom_builder_id :: ElBuilder
  }

data WasmState = WasmState
  { callbacks :: Map CallbackId (Expr -> Wasm ())
  , storage :: Set StoreId
  , queue :: Expr
  }

bindWasmResult :: forall a b. WasmResult a -> (a -> Wasm b) -> WasmEnv -> WasmState -> IO (WasmState, WasmResult b)
bindWasmResult r cont e s = case r of
  Pure a -> unWasm (cont a) e s
  Cmd cmd ->
    return (s, Interrupt cmd cont)
  FMap f i ->
    bindWasmResult i (cont . f) e s
  Interrupt cmd c2 -> do
    let
      cont' exp = Wasm \e s -> do
        (s', r') <- unWasm (c2 exp) e s
        bindWasmResult r' cont e s'
    return (s, Interrupt cmd cont')

runTillInterruption :: forall a. WasmEnv -> Wasm a -> IO (Either Expr a)
runTillInterruption e wasm = do
  s <- readIORef wasmStateRef
  (s', result) <- unWasm wasm e s `catch` \(e :: SomeException) ->
    -- UncaughtException command will not return a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s, unsafeCoerce (UncaughtException (Char8.pack (show e))))
  let
    g :: forall a. WasmResult a -> IO (Either Expr a)
    g r = case r of
      Pure a -> return (Right a)
      Cmd cmd -> return (Left cmd)
      FMap f i -> fmap (fmap f) (g i)
      Interrupt cmd cont -> do
        modifyIORef' continuationsRef (unsafeCoerce cont :)
        return $ Left cmd
  writeIORef wasmStateRef s' {queue = Num 0}
  eexpr <- g result
  case eexpr of
    Left e -> return $ Left $ Seq s'.queue e
    Right a
      | Num 0 <- s'.queue -> return $ Right a
      | otherwise -> do
        let cont (_::Expr) = return (unsafeCoerce a)
        modifyIORef' continuationsRef (cont:)
        return $ Left s'.queue

newVar :: Wasm StoreId
newVar = state \old ->
  let
    newVarId = maybe 0 succ (Set.lookupMax old.storage)
    newState = old { storage = Set.insert newVarId old.storage}
  in
    (newVarId, newState)

freeVar :: StoreId -> Wasm ()
freeVar varId = do
  modify \old -> old { storage = Set.delete varId old.storage}
  schedExp $ FreeVar varId

withVar :: (StoreId -> Wasm a) -> Wasm a
withVar f = do
  varId <- newVar
  result <- f varId
  result <$ freeVar varId

newCallback :: (Expr -> Wasm ()) -> Wasm CallbackId
newCallback k = state \old ->
  let
    (callbacks, callbackId) =
      insertNewCallback old.callbacks
    insertNewCallback old =
      let (newKey, new) = insertNewKey k old in (new, newKey)
    insertNewKey v m =
      let newKey = maybe 0 fst $ Map.lookupMax m in (newKey, Map.insert newKey v m)
  in
    (callbackId, old {callbacks})

evalExp :: Expr -> Wasm Expr
evalExp e = Wasm \_ s -> return (s, Cmd e)

schedExp :: Expr -> Wasm ()
schedExp e = Wasm \_ s -> return (s {queue = Seq s.queue e}, Pure ())

continuationsRef :: IORef [Expr -> Wasm Any]
continuationsRef = unsafePerformIO $ newIORef []

wasmStateRef :: IORef WasmState
wasmStateRef = unsafePerformIO $ newIORef WasmState
  { callbacks = Map.empty
  , storage = Set.empty
  , queue = Num 0
  }

handleCommand :: Wasm () -> DownCmd -> IO UpCmd
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
  ExecCallback _ _ _ -> do
    return $ Eval $ UncaughtException "Deprecated command ExecCallback"
  ExecCallbackVar arg callbackId -> do
    wasmState <- readIORef wasmStateRef
    let callback = Map.lookup callbackId wasmState.callbacks
    case callback of
      Nothing ->
        return $ Eval $ UncaughtException "Broken callbackId"
      Just c -> do
        result <- runTillInterruption wasmEnv (c arg)
        case result of
          Left exp -> return $ Eval exp
          Right _ -> return Exit
  where
    wasmEnv = WasmEnv (ElBuilder (LVar (StoreId (0))))

instance Functor Wasm where
  fmap f (Wasm g) = Wasm \e s -> fmap (\(s', r) -> (s', FMap f r)) (g e s)
  {-# INLINE fmap #-}

instance Applicative Wasm where
  pure a = Wasm \_ s -> return (s, Pure a)
  {-# INLINE pure #-}
  (<*>) mf ma = Wasm \e s -> do
    (s, r) <- unWasm mf e s
    bindWasmResult r (flip fmap ma) e s
  {-# INLINE (<*>) #-}

instance Monad Wasm where
  (>>=) ma mf = Wasm \e s -> do
    (s, r) <- unWasm ma e s
    bindWasmResult r mf e s
  {-# INLINE (>>=) #-}

instance MonadReader WasmEnv Wasm where
  local f (Wasm g) = Wasm \e -> g (f e)
  ask = Wasm \e s -> return (s, Pure e)

instance MonadState WasmState Wasm where
  state f = Wasm \_ s -> let (a, s') = f s in return (s', Pure a)
