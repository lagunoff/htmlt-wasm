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

-- | A computation capable of interacting with JavaScript.
newtype RJS a = RJS
  { unRJS :: ReactiveScope -> RjsState -> IO (RjsState, RjsResult a)
  }

data RjsResult a where
  PureResult :: a -> RjsResult a
  EvalResult :: Expr -> RjsResult JValue
  YieldResult :: CallbackId -> RjsResult JValue
  InterruptResult :: InterruptReason -> (JValue -> RJS b) -> RjsResult b
  FMapResult :: (a -> b) -> RjsResult a -> RjsResult b

data InterruptReason = EvalReason Expr | YieldReason CallbackId

data RjsState = RjsState
  { evaluation_queue :: [Expr]
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
  { evaluation_queue = []
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
newVar = reactive \e s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId (unQueueId (unReactiveScope e)) (unQueueId newQueueId)
  in
    (newVarId, s1)

newScope :: RJS ReactiveScope
newScope = reactive \e s0 ->
  let
    (scopeId, s1) = nextQueueId s0
    finalizerKey = CustomKey scopeId
    scope = ReactiveScope scopeId
    finalizers = Map.alter
      (Just . Map.insert finalizerKey
        (ScopeFinalizer scope) . fromMaybe Map.empty
      ) e s1.finalizers
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

newCallbackEvent :: (JValue -> RJS ()) -> RJS CallbackId
newCallbackEvent k = reactive \e s0 ->
  let
    (queueId, s1) = nextQueueId s0
    s2 = unsafeSubscribe (EventId queueId) k e s1
  in
    (CallbackId (unQueueId queueId), s2)

evalExpr :: Expr -> RJS JValue
evalExpr e = RJS \_ s -> return (s, EvalResult e)

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

yield :: CallbackId -> RJS JValue
yield callbackId = RJS \_ s -> return (s, YieldResult callbackId)

nextQueueId :: RjsState -> (QueueId, RjsState)
nextQueueId s =
  (s.id_supply, s {id_supply = succ s.id_supply})

unsafeSubscribe :: EventId -> (a -> RJS ()) -> ReactiveScope -> RjsState -> RjsState
unsafeSubscribe eventId k e s0 =
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
      ) . fromMaybe Map.empty) e s1.finalizers
  in
    s1 {subscriptions, finalizers}

installFinalizer :: FinalizerValue -> ReactiveScope -> RjsState -> (FinalizerKey, RjsState)
installFinalizer fin e s0 =
  let
    (finalizerId, s1) = nextQueueId s0
    finalizerKey = CustomKey finalizerId
    insertFin = Just . Map.insert finalizerKey fin . fromMaybe Map.empty
    finalizers = Map.alter insertFin e s1.finalizers
  in
    (finalizerKey, s1 {finalizers})

bindRjsResult
  :: forall a b. RjsResult a
  -> (a -> RJS b)
  -> ReactiveScope
  -> RjsState
  -> IO (RjsState, RjsResult b)
bindRjsResult r cont e s = case r of
  PureResult a -> unRJS (cont a) e s
  EvalResult expr ->
    return (s, InterruptResult (EvalReason expr) cont)
  YieldResult callbackId ->
    return (s, InterruptResult (YieldReason callbackId) cont)
  FMapResult f i ->
    bindRjsResult i (cont . f) e s
  InterruptResult reason c2 -> do
    let
      cont' exp = RJS \e s -> do
        (s', r') <- unRJS (c2 exp) e s
        bindRjsResult r' cont e s'
    return (s, InterruptResult reason cont')

class MonadReactive m where
  reactive :: (ReactiveScope -> RjsState -> (a, RjsState)) -> m a

reactive_ :: MonadReactive m => (ReactiveScope -> RjsState -> RjsState) -> m ()
reactive_ f = reactive \e s -> ((), f e s)
{-# INLINE reactive_ #-}

instance Functor RJS where
  fmap f (RJS g) = RJS \e s -> fmap h (g e s)
    where
      h (s, (PureResult a)) = (s, PureResult (f a))
      h (s, r) = (s, FMapResult f r)
  {-# INLINE fmap #-}

instance Applicative RJS where
  pure a = RJS \_ s -> return (s, PureResult a)
  {-# INLINE pure #-}
  (<*>) mf ma = RJS \e s -> do
    (s, r) <- unRJS mf e s
    bindRjsResult r (flip fmap ma) e s
  {-# INLINE (<*>) #-}

instance Monad RJS where
  (>>=) ma mf = RJS \e s -> do
    (s, r) <- unRJS ma e s
    bindRjsResult r mf e s
  {-# INLINE (>>=) #-}

instance MonadReader ReactiveScope RJS where
  local f (RJS g) = RJS \e -> g (f e)
  {-# INLINE local #-}
  ask = RJS \e s -> return (s, PureResult e)
  {-# INLINE ask #-}

instance MonadState RjsState RJS where
  state f = RJS \_ s -> let (a, s') = f s in return (s', PureResult a)
  {-# INLINE state #-}

instance MonadIO RJS where
  liftIO io = RJS \_ s -> fmap ((s,) . PureResult) io
  {-# INLINE liftIO #-}

instance MonadFix RJS where
  mfix f = RJS \e s -> mfix \ ~(_, a) -> unRJS (f (extractPure a)) e s
    where
      extractPure (PureResult a) = a
      extractPure _ = error
        "Asynchronous commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}

instance MonadReactive RJS where
  reactive f = RJS \e s -> let (a, s') = f e s in return (s', PureResult a)
  {-# INLINE reactive #-}
