module HtmlT.Wasm.Types where

import Control.Monad.Reader
import Control.Monad.Fix
import Control.Monad.State
import Data.Map (Map)
import Data.Set (Set)
import GHC.Exts
import GHC.Int
import GHC.Generics

import "this" HtmlT.Wasm.Protocol

data WAResult a where
  Pure :: a -> WAResult a
  Cmd :: Expr -> WAResult JValue
  Interrupt :: Expr -> (JValue -> WA b) -> WAResult b
  FMap :: (a -> b) -> WAResult a -> WAResult b

-- | A computation capable of interacting with JavaScript.
newtype WA a = WA
  { unWA :: WAEnv -> WAState -> IO (WAState, WAResult a)
  }

data WAEnv = WAEnv
  { dom_builder_id :: DomBuilder
  , save_current_element :: VarId
  , finalizer_ns :: FinalizerNs
  }

data WAState = WAState
  { var_storage :: Set VarId
  , evaluation_queue :: [Expr]
  , subscriptions :: Map EventId [(SubscriptionId, Any -> WA ())]
  , finalizers :: Map FinalizerNs (Map FinalizerKey FinalizerValue)
  , id_supply :: QueueId
  -- ^ Source of unique identifiers for EventId, SubscriptionId and
  -- VarId (potentially can lead to clashes if it overflows in a
  -- long-living application, TODO: is this a legitimate concern?)
  , transaction_queue :: Map QueueId (WA ())
  }

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
  | CustomFinalizer (WA ())
  | NamespaceFinalizer FinalizerNs
  | ParentNamespace FinalizerNs

newtype FinalizerNs = FinalizerNs {unFinalizerNs :: QueueId}
  deriving newtype (Eq, Ord, Num)

bindWasmResult :: forall a b. WAResult a -> (a -> WA b) -> WAEnv -> WAState -> IO (WAState, WAResult b)
bindWasmResult r cont e s = case r of
  Pure a -> unWA (cont a) e s
  Cmd cmd ->
    return (s, Interrupt cmd cont)
  FMap f i ->
    bindWasmResult i (cont . f) e s
  Interrupt cmd c2 -> do
    let
      cont' exp = WA \e s -> do
        (s', r') <- unWA (c2 exp) e s
        bindWasmResult r' cont e s'
    return (s, Interrupt cmd cont')

class MonadReactive m where
  reactive :: (WAEnv -> WAState -> (a, WAState)) -> m a

reactive_ :: MonadReactive m => (WAEnv -> WAState -> WAState) -> m ()
reactive_ f = reactive \e s -> ((), f e s)
{-# INLINE reactive_ #-}

instance Functor WA where
  fmap f (WA g) = WA \e s -> fmap h (g e s)
    where
      h (s, (Pure a)) = (s, Pure (f a))
      h (s, r) = (s, FMap f r)
  {-# INLINE fmap #-}

instance Applicative WA where
  pure a = WA \_ s -> return (s, Pure a)
  {-# INLINE pure #-}
  (<*>) mf ma = WA \e s -> do
    (s, r) <- unWA mf e s
    bindWasmResult r (flip fmap ma) e s
  {-# INLINE (<*>) #-}

instance Monad WA where
  (>>=) ma mf = WA \e s -> do
    (s, r) <- unWA ma e s
    bindWasmResult r mf e s
  {-# INLINE (>>=) #-}

instance MonadReader WAEnv WA where
  local f (WA g) = WA \e -> g (f e)
  {-# INLINE local #-}
  ask = WA \e s -> return (s, Pure e)
  {-# INLINE ask #-}

instance MonadState WAState WA where
  state f = WA \_ s -> let (a, s') = f s in return (s', Pure a)
  {-# INLINE state #-}

instance MonadIO WA where
  liftIO io = WA \_ s -> fmap ((s,) . Pure) io
  {-# INLINE liftIO #-}

instance MonadFix WA where
  mfix f = WA \e s -> mfix \ ~(_, a) -> unWA (f (extractPure a)) e s
    where
      extractPure (Pure a) = a
      extractPure _ = error
        "Asynchronous commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}

instance MonadReactive WA where
  reactive f = WA \e s -> let (a, s') = f e s in return (s', Pure a)
  {-# INLINE reactive #-}
