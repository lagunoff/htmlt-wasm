{-|
-}
module HtmlT.Wasm.Types where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Fix
import Control.Monad.State
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.Typeable
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

import "this" HtmlT.Wasm.Protocol

data WASMResult a where
  Pure :: a -> WASMResult a
  Cmd :: Expr -> WASMResult JValue
  Interrupt :: Expr -> (JValue -> WASM b) -> WASMResult b
  FMap :: (a -> b) -> WASMResult a -> WASMResult b

-- | A computation capable of interacting with JavaScript.
newtype WASM a = WASM
  { unWasm :: WASMEnv -> WASMState -> IO (WASMState, WASMResult a)
  }

data WASMEnv = WASMEnv
  { dom_builder_id :: DomBuilder
  , finalizer_ns :: FinalizerNs
  }

data WASMState = WASMState
  { var_storage :: Set VarId
  , evaluation_queue :: [Expr]
  , subscriptions :: Map EventId [(SubscriptionId, Any -> WASM ())]
  , finalizers :: Map FinalizerNs (Map FinalizerKey FinalizerValue)
  , id_supply :: QueueId
  -- ^ Source of unique identifiers for EventId, SubscriptionId and
  -- VarId (potentially can lead to clashes if it overflows in a
  -- long-living application, TODO: is this a legitimate concern?)
  , transaction_queue :: Map QueueId (WASM ())
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
  | CustomFinalizer (WASM ())
  | NamespaceFinalizer FinalizerNs
  | ParentNamespace FinalizerNs

newtype FinalizerNs = FinalizerNs {unFinalizerNs :: QueueId}
  deriving newtype (Eq, Ord, Num)

bindWasmResult :: forall a b. WASMResult a -> (a -> WASM b) -> WASMEnv -> WASMState -> IO (WASMState, WASMResult b)
bindWasmResult r cont e s = case r of
  Pure a -> unWasm (cont a) e s
  Cmd cmd ->
    return (s, Interrupt cmd cont)
  FMap f i ->
    bindWasmResult i (cont . f) e s
  Interrupt cmd c2 -> do
    let
      cont' exp = WASM \e s -> do
        (s', r') <- unWasm (c2 exp) e s
        bindWasmResult r' cont e s'
    return (s, Interrupt cmd cont')

class MonadReactive m where
  reactive :: (WASMEnv -> WASMState -> (a, WASMState)) -> m a

reactive_ :: MonadReactive m => (WASMEnv -> WASMState -> WASMState) -> m ()
reactive_ f = reactive \e s -> ((), f e s)
{-# INLINE reactive_ #-}

instance Functor WASM where
  fmap f (WASM g) = WASM \e s -> fmap h (g e s)
    where
      h (s, (Pure a)) = (s, Pure (f a))
      h (s, r) = (s, FMap f r)
  {-# INLINE fmap #-}

instance Applicative WASM where
  pure a = WASM \_ s -> return (s, Pure a)
  {-# INLINE pure #-}
  (<*>) mf ma = WASM \e s -> do
    (s, r) <- unWasm mf e s
    bindWasmResult r (flip fmap ma) e s
  {-# INLINE (<*>) #-}

instance Monad WASM where
  (>>=) ma mf = WASM \e s -> do
    (s, r) <- unWasm ma e s
    bindWasmResult r mf e s
  {-# INLINE (>>=) #-}

instance MonadReader WASMEnv WASM where
  local f (WASM g) = WASM \e -> g (f e)
  {-# INLINE local #-}
  ask = WASM \e s -> return (s, Pure e)
  {-# INLINE ask #-}

instance MonadState WASMState WASM where
  state f = WASM \_ s -> let (a, s') = f s in return (s', Pure a)
  {-# INLINE state #-}

instance MonadIO WASM where
  liftIO io = WASM \_ s -> fmap ((s,) . Pure) io
  {-# INLINE liftIO #-}

instance MonadFix WASM where
  mfix f = WASM \e s -> mfix \ ~(_, a) -> unWasm (f (extractPure a)) e s
    where
      extractPure (Pure a) = a
      extractPure _ = error
        "Asynchronous commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}

instance MonadReactive WASM where
  reactive f = WASM \e s -> let (a, s') = f e s in return (s', Pure a)
  {-# INLINE reactive #-}
