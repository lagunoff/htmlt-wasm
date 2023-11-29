module HtmlT.RJS where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
import GHC.Generics
import GHC.Int
import Unsafe.Coerce

import "this" HtmlT.Protocol
import "this" HtmlT.Protocol.JSVal

-- | A computation capable of interacting with JavaScript and supports
-- reactive operations like creating Events and subscribing to events
-- and dynamics
newtype RJS a = RJS
  { unRJS :: RjsState -> IO (RjsState, RjsResult a)
  }

data RjsResult a where
  PureResult :: a -> RjsResult a
  EvalResult :: Expr -> RjsResult JSVal
  YieldResult :: CallbackId -> RjsResult JSVal
  InterruptResult :: InterruptReason -> (JSVal -> RJS b) -> RjsResult b
  FMapResult :: (a -> b) -> RjsResult a -> RjsResult b

data InterruptReason = EvalReason Expr | YieldReason CallbackId

data RjsState = RjsState
  { reactive_scope :: ReactiveScope
  -- ^ ReactiveScope is an integer identifier to which finalizers are
  -- attached to. When freeScope is called it'll free other resources
  -- as well such as event subscriptions, variables, custom finalizers
  -- etc. It's in RjsState rather than in Reader like environment
  -- because it's easier to preserve its value across execution
  -- rounds.
  , evaluation_queue :: [Expr]
  , subscriptions :: Map EventId [(SubscriptionId, Any -> RJS ())]
  , finalizers :: Map ReactiveScope (Map FinalizerKey FinalizerValue)
  , id_supply :: QueueId
  -- ^ Source of unique identifiers for EventId, SubscriptionId and
  -- VarId (potentially can lead to clashes if it overflows in a
  -- long-living application, TODO: is this a legitimate concern?)
  , transaction_queue :: Map QueueId (RJS ())
  } deriving (Generic)

emptyRjsState :: RjsState
emptyRjsState = RjsState
  { reactive_scope = -1
  , evaluation_queue = []
  , subscriptions = Map.empty
  , finalizers = Map.empty
  , id_supply = 0
  , transaction_queue = Map.empty
  }

newtype QueueId = QueueId { unQueueId :: Int64 }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype EventId = EventId { unEventId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

data FinalizerKey
  = EventKey EventId
  | CustomKey QueueId
  deriving (Eq, Ord, Generic)

data FinalizerValue
  = SubscriptionSet (Set SubscriptionId)
  | CustomFinalizer (RJS ())
  | ScopeFinalizer ReactiveScope
  | ParentScope ReactiveScope

newtype ReactiveScope = ReactiveScope {unReactiveScope :: QueueId}
  deriving newtype (Eq, Ord, Num)

newVar :: RJS VarId
newVar = reactive \s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId s0.reactive_scope.unReactiveScope.unQueueId
      newQueueId.unQueueId
  in
    (newVarId, s1)

newScope :: RJS ReactiveScope
newScope = reactive \s0 ->
  let
    (scopeId, s1) = nextQueueId s0
    finalizerKey = CustomKey scopeId
    scope = ReactiveScope scopeId
    finalizers = Map.alter
      (Just . Map.insert finalizerKey
        (ScopeFinalizer scope) . fromMaybe Map.empty
      ) s0.reactive_scope s1.finalizers
  in
    (scope, s1 {finalizers})

type Subscriptions = Map EventId [(SubscriptionId, Any -> RJS ())]

type Finalizers = Map ReactiveScope (Map FinalizerKey FinalizerValue)

freeScope :: ReactiveScope -> RJS ()
freeScope rscope = do
  removedList <- state \s ->
    let
      (removed, finalizers0) = Map.alterF (,Nothing) rscope $ s.finalizers
      removedList = maybe [] Map.toList removed
      subscriptions = unsubscribe removedList s.subscriptions
      finalizers = unlinkParentScope removedList finalizers0
    in
      (removedList, s { subscriptions, finalizers })
  runCustomFinalizers removedList
  enqueueExpr $ FreeScope $ unQueueId $ unReactiveScope rscope
  where
    unsubscribe :: [(FinalizerKey, FinalizerValue)] -> Subscriptions -> Subscriptions
    unsubscribe [] !s = s
    unsubscribe ((EventKey e, SubscriptionSet u) : xs) !s =
      unsubscribe xs $
        Map.alter (mfilter (not . List.null) . Just . deleteSubs u . fromMaybe []) e s
    unsubscribe (_ : xs) !s = unsubscribe xs s

    unlinkParentScope :: [(FinalizerKey, FinalizerValue)] -> Finalizers -> Finalizers
    unlinkParentScope [] !s = s
    unlinkParentScope ((_, ParentScope p) : _) !s = -- Expecting at most one ParentScope
      Map.alter (fmap (Map.delete (CustomKey (unReactiveScope p)))) p s
    unlinkParentScope (_ : xs) !s = unlinkParentScope xs s

    runCustomFinalizers :: [(FinalizerKey, FinalizerValue)] -> RJS ()
    runCustomFinalizers [] = return ()
    runCustomFinalizers ((_, CustomFinalizer w) : xs) = w *> runCustomFinalizers xs
    runCustomFinalizers ((_, ScopeFinalizer n) : xs) =
      freeScope n *> runCustomFinalizers xs
    runCustomFinalizers ((_, _) : xs) = runCustomFinalizers xs

    deleteSubs _ss [] = []
    deleteSubs ss ((s, c):xs)
      | Set.member s ss = xs
      | otherwise = (s, c) : deleteSubs ss xs

localScope :: ReactiveScope -> RJS a -> RJS a
localScope reactive_scope (RJS rjs) = RJS \s0 -> do
  (s1, r) <- rjs s0 {reactive_scope}
  return (s1 {reactive_scope = s0.reactive_scope}, r)

newCallback :: (JSVal -> RJS ()) -> RJS CallbackId
newCallback k = reactive \s0 ->
  let
    (queueId, s1) = nextQueueId s0
    s2 = unsafeSubscribe (EventId queueId) k s1
  in
    (CallbackId (unQueueId queueId), s2)

releaseCallback :: CallbackId -> RJS ()
releaseCallback callbackId = modify \s ->
  let
    eventId = EventId (QueueId (unCallbackId callbackId))
    -- TODO: Does it makes sence to also remove all the finalizers
    -- associated with the eventId?
    subscriptions = Map.delete eventId s.subscriptions
  in
    s {subscriptions}

evalExpr :: Expr -> RJS JSVal
evalExpr e = RJS \s -> return (s, EvalResult e)

enqueueExpr :: Expr -> RJS ()
enqueueExpr e = modify \s ->
  s {evaluation_queue = e : s.evaluation_queue}

enqueueIfAlive :: ReactiveScope -> Expr -> RJS ()
enqueueIfAlive rscope e = modify \s ->
  let
    evaluation_queue =
      if isJust (Map.lookup rscope s.finalizers)
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

flushQueue :: RJS ()
flushQueue = do
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  void $ evalExpr $ RevSeq queue

yield :: CallbackId -> RJS JSVal
yield callbackId = RJS \s -> return (s, YieldResult callbackId)

nextQueueId :: RjsState -> (QueueId, RjsState)
nextQueueId s =
  (s.id_supply, s {id_supply = succ s.id_supply})

unsafeSubscribe :: EventId -> (a -> RJS ()) -> RjsState -> RjsState
unsafeSubscribe eventId k s0 =
  let
    (subId, s1) = nextQueueId s0
    newSubscription = (SubscriptionId subId, k . unsafeCoerce)
    f (SubscriptionSet ss1) (SubscriptionSet ss2) = SubscriptionSet (ss1 <> ss2)
    -- Unreacheable because EventKey always should map into
    -- SubscriptionSet
    f _ s = s
    subscriptions = Map.alter (Just . (newSubscription :) . fromMaybe [])
      eventId s1.subscriptions
    finalizers = Map.alter (Just .
      (Map.insertWith f (EventKey eventId)
        (SubscriptionSet (Set.singleton (SubscriptionId subId)))
      ) . fromMaybe Map.empty) s0.reactive_scope s1.finalizers
  in
    s1 {subscriptions, finalizers}

installFinalizer :: FinalizerValue -> RjsState -> (FinalizerKey, RjsState)
installFinalizer fin s0 =
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = CustomKey finalizerId
    insertFin = Just . Map.insert finalizerKey fin . fromMaybe Map.empty
    finalizers = Map.alter insertFin s0.reactive_scope s1.finalizers
  in
    (finalizerKey, s1 {finalizers})

bindRjsResult
  :: forall a b. RjsResult a
  -> (a -> RJS b)
  -> RjsState
  -> IO (RjsState, RjsResult b)
bindRjsResult r cont s = case r of
  PureResult a -> unRJS (cont a) s
  EvalResult expr ->
    return (s, InterruptResult (EvalReason expr) cont)
  YieldResult callbackId ->
    return (s, InterruptResult (YieldReason callbackId) cont)
  FMapResult f i ->
    bindRjsResult i (cont . f) s
  InterruptResult reason c2 -> do
    let
      cont' exp = RJS \s -> do
        (s', r') <- unRJS (c2 exp) s
        bindRjsResult r' cont s'
    return (s, InterruptResult reason cont')

class MonadReactive m where
  reactive :: (RjsState -> (a, RjsState)) -> m a

reactive_ :: MonadReactive m => (RjsState -> RjsState) -> m ()
reactive_ f = reactive \s -> ((), f s)
{-# INLINE reactive_ #-}

instance Functor RJS where
  fmap f (RJS g) = RJS \s -> fmap h (g s)
    where
      h (s, (PureResult a)) = (s, PureResult (f a))
      h (s, r) = (s, FMapResult f r)
  {-# INLINE fmap #-}

instance Applicative RJS where
  pure a = RJS \s -> return (s, PureResult a)
  {-# INLINE pure #-}
  (<*>) mf ma = RJS \s -> do
    (s, r) <- unRJS mf s
    bindRjsResult r (flip fmap ma) s
  {-# INLINE (<*>) #-}

instance Monad RJS where
  (>>=) ma mf = RJS \s -> do
    (s, r) <- unRJS ma s
    bindRjsResult r mf s
  {-# INLINE (>>=) #-}

instance MonadState RjsState RJS where
  state f = RJS \s -> let (a, s') = f s in return (s', PureResult a)
  {-# INLINE state #-}

instance MonadIO RJS where
  liftIO io = RJS \s -> fmap ((s,) . PureResult) io
  {-# INLINE liftIO #-}

instance MonadFix RJS where
  mfix f = RJS \s -> mfix \ ~(_, a) -> unRJS (f (extractPure a)) s
    where
      extractPure (PureResult a) = a
      extractPure _ = error
        "Asynchronous commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}

instance MonadReactive RJS where
  reactive f = RJS \s -> let (a, s') = f s in return (s', PureResult a)
  {-# INLINE reactive #-}
