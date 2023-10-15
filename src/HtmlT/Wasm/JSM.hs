module HtmlT.Wasm.JSM where

import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
import GHC.Generics
import GHC.Int

import "this" HtmlT.Wasm.Protocol

data JSMResult a where
  Pure :: a -> JSMResult a
  Cmd :: Expr -> JSMResult JValue
  Interrupt :: Expr -> (JValue -> JSM b) -> JSMResult b
  FMap :: (a -> b) -> JSMResult a -> JSMResult b

-- | A computation capable of interacting with JavaScript.
newtype JSM a = JSM
  { unJSM :: JSMEnv -> JSMState -> IO (JSMState, JSMResult a)
  }

data JSMEnv = JSMEnv
  { finalizer_ns :: FinalizerNs
  }

data JSMState = JSMState
  { var_storage :: Set VarId
  , evaluation_queue :: [Expr]
  , subscriptions :: Map EventId [(SubscriptionId, Any -> JSM ())]
  , finalizers :: Map FinalizerNs (Map FinalizerKey FinalizerValue)
  , id_supply :: QueueId
  -- ^ Source of unique identifiers for EventId, SubscriptionId and
  -- VarId (potentially can lead to clashes if it overflows in a
  -- long-living application, TODO: is this a legitimate concern?)
  , transaction_queue :: Map QueueId (JSM ())
  }

emptyWAState :: JSMState
emptyWAState = JSMState
  { var_storage = Set.fromList [0, 1]
  , evaluation_queue = []
  , subscriptions = Map.empty
  , finalizers = Map.empty
  , id_supply = 0
  , transaction_queue = Map.empty
  }

class MonadJSM m where
  wasm :: (JSMEnv -> JSMState -> (JSMState, JSMResult a)) -> m a

instance MonadJSM JSM where
  wasm f = JSM \e s -> pure (f e s)
  {-# INLINE wasm #-}

newtype QueueId = QueueId { unQueueId :: Int64 }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype EventId = EventId { unEventId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

data FinalizerKey
  = FinalizerEventId EventId
  | FinalizerCustomId QueueId
  deriving (Eq, Ord, Generic)

data FinalizerValue
  = SubscriptionSet (Set SubscriptionId)
  | CustomFinalizer (JSM ())
  | NamespaceFinalizer FinalizerNs
  | ParentNamespace FinalizerNs

newtype FinalizerNs = FinalizerNs {unFinalizerNs :: QueueId}
  deriving newtype (Eq, Ord, Num)

bindWasmResult :: forall a b. JSMResult a -> (a -> JSM b) -> JSMEnv -> JSMState -> IO (JSMState, JSMResult b)
bindWasmResult r cont e s = case r of
  Pure a -> unJSM (cont a) e s
  Cmd cmd ->
    return (s, Interrupt cmd cont)
  FMap f i ->
    bindWasmResult i (cont . f) e s
  Interrupt cmd c2 -> do
    let
      cont' exp = JSM \e s -> do
        (s', r') <- unJSM (c2 exp) e s
        bindWasmResult r' cont e s'
    return (s, Interrupt cmd cont')

class MonadReactive m where
  reactive :: (JSMEnv -> JSMState -> (a, JSMState)) -> m a

reactive_ :: MonadReactive m => (JSMEnv -> JSMState -> JSMState) -> m ()
reactive_ f = reactive \e s -> ((), f e s)
{-# INLINE reactive_ #-}

instance Functor JSM where
  fmap f (JSM g) = JSM \e s -> fmap h (g e s)
    where
      h (s, (Pure a)) = (s, Pure (f a))
      h (s, r) = (s, FMap f r)
  {-# INLINE fmap #-}

instance Applicative JSM where
  pure a = JSM \_ s -> return (s, Pure a)
  {-# INLINE pure #-}
  (<*>) mf ma = JSM \e s -> do
    (s, r) <- unJSM mf e s
    bindWasmResult r (flip fmap ma) e s
  {-# INLINE (<*>) #-}

instance Monad JSM where
  (>>=) ma mf = JSM \e s -> do
    (s, r) <- unJSM ma e s
    bindWasmResult r mf e s
  {-# INLINE (>>=) #-}

instance MonadReader JSMEnv JSM where
  local f (JSM g) = JSM \e -> g (f e)
  {-# INLINE local #-}
  ask = JSM \e s -> return (s, Pure e)
  {-# INLINE ask #-}

instance MonadState JSMState JSM where
  state f = JSM \_ s -> let (a, s') = f s in return (s', Pure a)
  {-# INLINE state #-}

instance MonadIO JSM where
  liftIO io = JSM \_ s -> fmap ((s,) . Pure) io
  {-# INLINE liftIO #-}

instance MonadFix JSM where
  mfix f = JSM \e s -> mfix \ ~(_, a) -> unJSM (f (extractPure a)) e s
    where
      extractPure (Pure a) = a
      extractPure _ = error
        "Asynchronous commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}

instance MonadReactive JSM where
  reactive f = JSM \e s -> let (a, s') = f e s in return (s', Pure a)
  {-# INLINE reactive #-}
