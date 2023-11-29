module HtmlT.Base where

import Control.Exception
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.Exts
import GHC.Generics
import Unsafe.Coerce

import "this" HtmlT.Event
import "this" HtmlT.Protocol
import "this" HtmlT.RJS
import "this" HtmlT.Protocol.JSVal

data RjsInstance = RjsInstance
  { continuations_ref :: IORef [JSVal -> RJS Any]
  , async_continuations_ref :: IORef (Map CallbackId (JSVal -> RJS Any))
  , rjs_state_ref :: IORef RjsState
  } deriving (Generic)

newRjsInstance :: IO RjsInstance
newRjsInstance = do
  rjs_state_ref <- newIORef emptyRjsState
  async_continuations_ref <- newIORef Map.empty
  continuations_ref <- newIORef []
  return RjsInstance {rjs_state_ref, async_continuations_ref, continuations_ref}

runUntillInterruption
  :: RjsInstance
  -> ReactiveScope
  -> RJS a
  -> IO (Either HaskellMessage a)
runUntillInterruption inst e rjs = do
  s0 <- readIORef inst.rjs_state_ref
  (s1, result) <- unRJS rjs e s0 `catch` \(e :: SomeException) ->
    -- UncaughtException command never returns a value from JS side,
    -- therefore we can coerce the result to any type
    pure (s0, coerceResult (EvalResult (UncaughtException (Text.pack (show e)))))
  let
    g :: forall a. RjsResult a -> IO (Either InterruptReason a)
    g r = case r of
      PureResult a -> return (Right a)
      EvalResult expr -> return (Left (EvalReason expr))
      YieldResult callbackId -> return (Left (YieldReason callbackId))
      FMapResult f i -> fmap (fmap f) (g i)
      InterruptResult reason cont -> do
        case reason of
          YieldReason callbackId ->
            modifyIORef' inst.async_continuations_ref
              $ Map.insert callbackId $ unsafeCoerce cont
          EvalReason _ ->
            modifyIORef' inst.continuations_ref (unsafeCoerce cont :)
        return $ Left reason
  writeIORef inst.rjs_state_ref s1 {evaluation_queue = []}
  eexpr <- g result
  case eexpr of
    Left (EvalReason e) ->
      return $ Left $ EvalExpr $ RevSeq $ e:s1.evaluation_queue
    Left (YieldReason _cbId) ->
      return $ Left $ Yield $ RevSeq s1.evaluation_queue
    Right a
      | [] <- s1.evaluation_queue -> return $ Right a
      | otherwise -> do
        let cont (_::JSVal) = return (unsafeCoerce a)
        modifyIORef' inst.continuations_ref (cont:)
        return $ Left $ EvalExpr $ RevSeq s1.evaluation_queue
  where
    coerceResult :: forall a b. RjsResult a -> RjsResult b
    coerceResult = unsafeCoerce

handleMessage
  :: RjsInstance
  -> (StartFlags -> RJS ())
  -> JavaScriptMessage
  -> IO HaskellMessage
handleMessage inst jsMain = handleMessage' inst jsMain . Right

handleMessage'
  :: RjsInstance
  -> (StartFlags -> RJS ())
  -> Either (RJS ()) JavaScriptMessage
  -> IO HaskellMessage
handleMessage' inst jsMain = \case
  Right (Start startFlags) -> do
    writeIORef inst.continuations_ref []
    writeIORef inst.rjs_state_ref emptyRjsState
    result <- runUntillInterruption inst rootScope (jsMain startFlags)
    case result of
      Left haskMsg -> return haskMsg
      Right () -> return Exit
  Right (Return jval) -> do
    mContinuation <- atomicModifyIORef' inst.continuations_ref \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case mContinuation of
      Nothing ->
        return $ EvalExpr $ UncaughtException "Synchronous continuation is missing"
      Just c -> do
        result <- runUntillInterruption inst rootScope (c jval)
        case result of
          Left haskMsg -> return haskMsg
          Right _ -> return Exit
  Right (TriggerEventMsg arg callbackId) -> do
    let
      eventId = EventId (QueueId callbackId.unCallbackId)
      rjs = unsafeTrigger eventId arg
    result <- runUntillInterruption inst rootScope (dynStep rjs)
    case result of
      Left haskMsg -> return haskMsg
      Right _ -> return Exit
  Right (AsyncCallbackMsg arg callbackId) -> do
    mContinuation <- atomicModifyIORef' inst.async_continuations_ref \c ->
      (Map.delete callbackId c, Map.lookup callbackId c)
    case mContinuation of
      Nothing ->
        return $ EvalExpr $ UncaughtException "Asynchronous continuation is missing"
      Just c -> do
        result <- runUntillInterruption inst rootScope (c arg)
        case result of
          Left haskMsg -> return haskMsg
          Right _ -> return Exit
  Right BeforeUnload -> do
    result <- runUntillInterruption inst rootScope (freeScope rootScope)
    case result of
      Left haskMsg -> return haskMsg
      Right _ -> return Exit
  Left jsAction -> do
    result <- runUntillInterruption inst rootScope jsAction
    case result of
      Left haskMsg -> return haskMsg
      Right () -> return Exit
  where
    rootScope = ReactiveScope (-1)
