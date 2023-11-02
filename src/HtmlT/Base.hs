module HtmlT.Base where

import Control.Exception
import Data.IORef
import GHC.Exts
import GHC.Generics
import Unsafe.Coerce

import "this" HtmlT.Event
import "this" HtmlT.RJS
import "this" HtmlT.Protocol
import "this" HtmlT.Protocol.Utf8 qualified as Utf8

data RjsInstance = RjsInstance
  { continuations_ref :: IORef [JValue -> RJS Any]
  , rjs_state_ref :: IORef RjsState
  } deriving (Generic)

runUntillInterruption :: RjsInstance -> ReactiveScope -> RJS a -> IO (Either Expr a)
runUntillInterruption opt e wasm = do
  s0 <- readIORef opt.rjs_state_ref
  (s1, result) <- unRJS wasm e s0 `catch` \(e :: SomeException) ->
    -- UncaughtException command never returns a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s0, coerceResult (EvalResult (UncaughtException (Utf8.pack (show e)))))
  let
    g :: forall a. RjsResult a -> IO (Either Expr a)
    g r = case r of
      PureResult a -> return (Right a)
      EvalResult cmd -> return (Left cmd)
      FMapResult f i -> fmap (fmap f) (g i)
      InterruptResult cmd cont -> do
        modifyIORef' opt.continuations_ref (unsafeCoerce cont :)
        return $ Left cmd
  writeIORef opt.rjs_state_ref s1 {evaluation_queue = []}
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
    coerceResult :: forall a b. RjsResult a -> RjsResult b
    coerceResult = unsafeCoerce

handleMessage :: RjsInstance -> (StartFlags -> RJS ()) -> JavaScriptMessage -> IO HaskellMessage
handleMessage inst jsMain = \case
  Start startFlags -> do
    writeIORef inst.continuations_ref []
    writeIORef inst.rjs_state_ref emptyRjsState
    result <- runUntillInterruption inst rootScope (jsMain startFlags)
    case result of
      Left exp -> return $ EvalExpr exp
      Right () -> return Exit
  Return jval -> do
    tipCont <- atomicModifyIORef' inst.continuations_ref \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case tipCont of
      Nothing ->
        return $ EvalExpr $ UncaughtException "Protocol violation: continuation is missing"
      Just c -> do
        result <- runUntillInterruption inst rootScope (c jval)
        case result of
          Left exp -> return $ EvalExpr exp
          Right _ -> return Exit
  ExecCallbackCommand arg callbackId -> do
    let
      eventId = EventId (QueueId (unCallbackId callbackId))
      wasm = unsafeTrigger eventId arg
    result <- runUntillInterruption inst rootScope (dynStep wasm)
    case result of
      Left exp -> return $ EvalExpr exp
      Right _ -> return Exit
  BeforeUnload -> do
    result <- runUntillInterruption inst rootScope (finalizeNamespace rootScope)
    case result of
      Left exp -> return $ EvalExpr exp
      Right _ -> return Exit
  where
    rootScope = ReactiveScope (-1)
