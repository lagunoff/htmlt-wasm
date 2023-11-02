module HtmlT.Reactive where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple
import GHC.Exts
import GHC.Int
import GHC.Generics
import Unsafe.Coerce

newtype ReactiveT m a = ReactiveT
  {unReactiveT :: ReactiveScope -> ReactiveState m -> m (ReactiveState m, a)}

newtype ReactiveScope = ReactiveScope {unReactiveScope :: QueueId}
  deriving newtype (Eq, Ord)

data ReactiveState m = ReactiveState
  { subscriptions :: Map EventId [(SubscriptionId, Any -> ReactiveT m ())]
  , finalizers :: Map ReactiveScope (Map FinalizerKey (FinalizerValue m))
  , id_supply :: QueueId
  , transaction_queue :: Map QueueId (ReactiveT m ())
  } deriving (Generic)

emptyReactiveState :: ReactiveState m
emptyReactiveState = ReactiveState
  { subscriptions = Map.empty
  , finalizers = Map.empty
  , id_supply = 0
  , transaction_queue = Map.empty
  }

newtype Event m a = Event { unEvent :: (a -> ReactiveT m ()) -> ReactiveT m () }

newtype EventId = EventId { unEventId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: QueueId }
  deriving newtype (Eq, Ord, Show, Num, Enum)

data FinalizerKey
  = FinalizerEventId EventId
  | FinalizerCustomId QueueId
  deriving (Eq, Ord, Generic)

data FinalizerValue m
  = SubscriptionSet (Set SubscriptionId)
  | CustomFinalizer (ReactiveT m ())
  | NamespaceFinalizer ReactiveScope
  | ParentNamespace ReactiveScope

-- | Contains a value that is subject to change over time. Provides
-- operations for reading the current value ('readDyn') and
-- subscribing to its future changes ('updates').
data Dynamic m a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value. Use public alias 'readDyn' instead
  , dynamic_updates :: Event m a
  -- ^ Event that fires when the value changes. Use public alias
  -- 'updates' instead
  }

-- | A mutable variable that allows for subscription to new values. It
-- shares a similar API to 'IORef' (see 'readRef', 'writeRef',
-- 'modifyRef')
data DynRef m a = DynRef
  { dynref_value :: Dynamic m a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dynref_modifier :: Modifier m a
  -- ^ Funtion to update the value
  }

-- | Function that updates the value inside the 'DynRef'
newtype Modifier m a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> ReactiveT m r
  -- ^ 'Bool' argument controls whether the modification should
  -- trigger an update event. It's possible to update the 'DynRef'
  -- without notifying the subscribers for optimization purposes, in
  -- cases when you know that all changes already been reflected in
  -- the DOM
  }

-- execRJS :: ReactiveScope -> (ReactiveState, JSState) -> ReactiveT m a -> IO ((ReactiveState, JSState), JSResult a)
-- execRJS rs rsjs act = do
--   (jsst, res) <- unJS $ unReactiveT act rs

unsafeSubscribe :: EventId -> (a -> ReactiveT m ()) -> ReactiveScope -> ReactiveState m -> ReactiveState m
unsafeSubscribe eventId k e s0 =
  let
    (subId, s1) = nextQueueId1 s0
    newSubscription = (SubscriptionId subId, k . unsafeCoerce)
    f (SubscriptionSet s1) (SubscriptionSet s2) = SubscriptionSet (s1 <> s2)
    -- Unreacheable because FinalizerEventId always should map into
    -- SubscriptionSet
    f _ s = s
    subscriptions = Map.alter (Just . (newSubscription :) . fromMaybe [])
      eventId s1.subscriptions
    finalizers = Map.alter (Just .
      (Map.insertWith f (FinalizerEventId eventId)
        (SubscriptionSet (Set.singleton (SubscriptionId subId)))
      ) . fromMaybe Map.empty) e s1.finalizers
  in
    s1 {subscriptions, finalizers}

unsafeTrigger :: EventId -> a -> ReactiveT m ()
unsafeTrigger eventId a = defer (unEventId eventId) do
  callbacks <- gets $ fromMaybe [] .
    Map.lookup eventId . (.subscriptions)
  forM_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

newtype QueueId = QueueId { unQueueId :: Int64 }
  deriving newtype (Eq, Ord, Show, Num, Enum)

nextQueueId1 :: ReactiveState m -> (QueueId, ReactiveState m)
nextQueueId1 s =
  (s.id_supply, s {id_supply = succ s.id_supply})

installFinalizer :: FinalizerValue m -> ReactiveScope -> ReactiveState m -> (FinalizerKey, ReactiveState m)
installFinalizer fin e s0 =
  let
    (finalizerId, s1) = nextQueueId1 s0
    finalizerKey = FinalizerCustomId finalizerId
    insertFin = Just . Map.insert finalizerKey fin . fromMaybe Map.empty
    finalizers = Map.alter insertFin e s1.finalizers
  in
    (finalizerKey, s1 {finalizers})

-- | Defers a computation (typically an event firing) until the end of
-- the current reactive transaction. This allows for the avoidance of
-- double firing of events constructed from multiple other events.
defer :: QueueId -> ReactiveT m () -> ReactiveT m ()
defer k act = modify \s ->
  s {transaction_queue = Map.insert k act s.transaction_queue}

newEvent :: ReactiveT m (Event m a, a -> ReactiveT m ())
newEvent = state \s0 ->
  let
    (eventId, s1) = nextQueueId1 s0
    event = Event (reactive_ . unsafeSubscribe (EventId eventId))
    trig = unsafeTrigger (EventId eventId)
  in
    ((event, trig), s1)

newRef :: a -> ReactiveT m (DynRef m a)
newRef initial = do
  ioRef <- liftIO $ newIORef initial
  (event, push) <- newEvent
  let
    modifier = Modifier \u f -> do
      (new, result) <- liftIO $ atomicModifyIORef' ioRef \old ->
        let (new, result) = f old in
          (new, (new, result))
      when u $ push new
      return result
  return DynRef
    { dynref_value = Dynamic (readIORef ioRef) event
    , dynref_modifier = modifier
    }

-- | Create a Dynamic that never changes its value
constDyn :: a -> Dynamic m a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event m a
never = Event \_ -> return ()
-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial value"
-- > transactionWrite ref "New value"
-- > readRef ref
-- "New value"
writeRef :: DynRef m a -> a -> ReactiveT m ()
writeRef ref a = modifyRef ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef m a -> m a
readRef = readDyn . dynref_value

-- | Update a 'DynRef' by applying given function to the current value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: DynRef m a -> (a -> a) -> ReactiveT m ()
modifyRef (DynRef _ (Modifier mod)) f = mod True $ (,()) . f

-- | Update a 'DynRef' with first field of the tuple and return back
-- the second field. The name is intended to be similar to
-- 'atomicModifyIORef' but there are no atomicity guarantees
-- whatsoever
atomicModifyRef :: DynRef m a -> (a -> (a, r)) -> ReactiveT m r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef m a -> Dynamic m a
fromRef = dynref_value

-- | Read the value held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic m a -> m a
readDyn = liftIO . dynamic_read

-- | Extract the updates Event from a 'Dynamic'
updates :: Dynamic m a -> Event m a
updates = dynamic_updates

-- | Attach a listener to the event and return an action to detach the
-- listener
subscribe :: Event m a -> (a -> ReactiveT m ()) -> ReactiveT m ()
subscribe (Event s) k = s k

-- | Executes an action currently held inside the 'Dynamic' and every
-- time the value changes.
performDyn :: Dynamic m (ReactiveT m ()) -> ReactiveT m ()
performDyn d = do
  join $ liftIO $ dynamic_read d
  subscribe d.dynamic_updates id

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic m a -> Dynamic m a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- TODO: holdUniqDynBy could be a misleading name, because it won't
-- hold the value for the whole Dynamic, instead it will perform the
-- comparison for each subscription
-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic m a -> Dynamic m a
holdUniqDynBy equalFn Dynamic{..} = Dynamic dynamic_read
  (Event \k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates \new -> do
      old <- liftIO $ atomicModifyIORef' oldRef (new,)
      unless (old `equalFn` new) $ k new
  )

type Subscriptions m = Map EventId [(SubscriptionId, Any -> ReactiveT m ())]

type Finalizers m = Map ReactiveScope (Map FinalizerKey (FinalizerValue m))

finalizeReactiveScope :: ReactiveScope -> ReactiveT m ()
finalizeReactiveScope ns = do
  removedList <- state \s ->
    let
      (removed, finalizers0) = Map.alterF (,Nothing) ns $ s.finalizers
      removedList = maybe [] Map.toList removed
      subscriptions = unsubscribe removedList s.subscriptions
      finalizers = unlinkParentScope removedList finalizers0
    in
      (removedList, s { subscriptions, finalizers })
  runCustomFinalizers removedList
  where
    unsubscribe :: [(FinalizerKey, FinalizerValue)] -> Subscriptions -> Subscriptions
    unsubscribe [] !s = s
    unsubscribe ((FinalizerEventId e, SubscriptionSet u) : xs) !s =
      unsubscribe xs $
        Map.alter (mfilter (not . List.null) . Just . deleteSubs u . fromMaybe []) e s
    unsubscribe (_ : xs) !s = unsubscribe xs s

    unlinkParentScope :: [(FinalizerKey, FinalizerValue)] -> Finalizers -> Finalizers
    unlinkParentScope [] !s = s
    unlinkParentScope ((_, ParentNamespace p) : _) !s = -- Expecting at most one ParentNamespace
      Map.alter (fmap (Map.delete (FinalizerCustomId (unReactiveScope p)))) p s
    unlinkParentScope (_ : xs) !s = unlinkParentScope xs s

    runCustomFinalizers :: [(FinalizerKey, FinalizerValue)] -> ReactiveT m ()
    runCustomFinalizers [] = return ()
    runCustomFinalizers ((_, CustomFinalizer w) : xs) = w *> runCustomFinalizers xs
    runCustomFinalizers ((_, NamespaceFinalizer n) : xs) =
      finalizeReactiveScope n *> runCustomFinalizers xs
    runCustomFinalizers ((_, _) : xs) = runCustomFinalizers xs

    deleteSubs _ss [] = []
    deleteSubs ss ((s, c):xs)
      | Set.member s ss = xs
      | otherwise = (s, c) : deleteSubs ss xs

newReactiveScope :: ReactiveT m ReactiveScope
newReactiveScope = reactive \e s0 ->
  let
    (namespaceId, s1) = nextQueueId1 s0
    finalizerKey = FinalizerCustomId namespaceId
    namespace = ReactiveScope namespaceId
    finalizers = Map.alter
      (Just . Map.insert finalizerKey
        (NamespaceFinalizer namespace) . fromMaybe Map.empty
      ) e s1.finalizers
  in
    (namespace, s1 {finalizers})

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event. As
-- a general guideline, if the function @f! is inexpensive, choose
-- @fmap f@. Otherwise, consider using @mapDyn f@.
mapDyn
  :: (a -> b)
  -> Dynamic m a
  -> ReactiveT m (Dynamic m b)
mapDyn fun adyn = do
  initialA <- liftIO $ dynamic_read adyn
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (fun initialA)
  eventId <- state nextQueueId1
  let
    updates = Event (reactive_ . unsafeSubscribe (EventId eventId))
    fire = defer eventId do
      newB <- liftIO $ fun <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      unsafeTrigger (EventId eventId) newB
  dynamic_updates adyn `subscribe` \newA -> do
    liftIO $ writeIORef latestA newA
    defer eventId fire
  return $ Dynamic (readIORef latestB) updates

-- | Takes a list of Dynamics and a function to generate the
-- output. The positions of elements in the list of [Any] received by
-- the function always correspond to the positions of [Dynamic Any]
-- from which these values were generated. The Dynamic created by this
-- function will fire at most once per transaction, and only if any of
-- the input Dynamics change their values.
unsafeMapDynN
  :: ([Any] -> IO a)
  -- ^ Construct the output value, from list of input values from
  -- corresponding positions of given Dynamics
  -> [Dynamic m Any]
  -- ^ List of input Dynamics
  -> ReactiveT m (Dynamic m a)
unsafeMapDynN fun dyns = do
  -- TODO: Try if list of IORefs is better than IORef of list
  initialInputs <- liftIO $ mapM (.dynamic_read) dyns
  initialOutput <- liftIO $ fun initialInputs
  latestInputsRef <- liftIO $ newIORef initialInputs
  latestOutputRef <- liftIO $ newIORef initialOutput
  eventId <- state nextQueueId1
  let
    fire = defer eventId do
      newOutput <- liftIO $ fun =<< readIORef latestInputsRef
      liftIO $ writeIORef latestOutputRef newOutput
      unsafeTrigger (EventId eventId) newOutput
    updates = Event (reactive_ . unsafeSubscribe (EventId eventId))
    updateList _ _ [] = []
    updateList 0 a (_:xs) = a:xs
    updateList n a (x:xs) = x : updateList (pred n) a xs
  forM_ (zip [0..] dyns) \(i::Int, adyn) -> do
    adyn.dynamic_updates `subscribe` \newVal -> do
      liftIO $ modifyIORef latestInputsRef $ updateList i newVal
      defer eventId fire
  return $ Dynamic (readIORef latestOutputRef) updates

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Apply a lens to the value inside 'DynRef'
lensMap :: forall s a m. Lens' s a -> DynRef m s -> DynRef m a
lensMap l (DynRef sdyn (Modifier smod)) =
  DynRef adyn (Modifier amod)
    where
      adyn = Dynamic
        (fmap (getConst . l Const) $ dynamic_read sdyn)
        (fmap (getConst . l Const) $ dynamic_updates sdyn)
      amod :: forall r. Bool -> (a -> (a, r)) -> ReactiveT m r
      amod u f = smod u $ swap . l (swap . f)

-- | Run a reactive transaction.
dynStep :: ReactiveT m a -> ReactiveT m a
dynStep act = loop0 act where
  loop0 :: ReactiveT m a -> ReactiveT m a
  loop0 act = do
    r <- act
    loop1 =<< gets (.transaction_queue)
    return r
  loop1 :: Map QueueId (ReactiveT m ()) -> ReactiveT m ()
  loop1 q =
    case Map.minViewWithKey q of
      Nothing -> return ()
      Just ((_, newAct), newQueue) -> do
        modify \s -> s {transaction_queue = newQueue}
        newAct
        loop1 =<< gets (.transaction_queue)

instance Functor (Event m) where
  fmap f (Event s) = Event \k -> s . (. f) $ k

instance Functor (Dynamic m) where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative (Dynamic m) where
  pure = constDyn
  (<*>) df da =
    let
      updatesEvent = Event \k -> mdo
        let
          fire newF newA = defer eventId do
            f <- liftIO $ maybe (readDyn df) pure newF
            a <- liftIO $ maybe (readDyn da) pure newA
            k (f a)
        unEvent (updates df) \f -> fire (Just f) Nothing
        unEvent (updates da) \a -> fire Nothing (Just a)
        eventId <- state nextQueueId1
        return ()
    in
      Dynamic
        { dynamic_read = liftA2 ($) (dynamic_read df) (dynamic_read da)
        , dynamic_updates = updatesEvent
        }

-- class MonadReactive m where
--   reactive :: (ReactiveScope -> ReactiveState m -> (a, ReactiveState m)) -> m a

-- reactive_ :: MonadReactive m => (ReactiveScope -> ReactiveState -> ReactiveState) -> m ()
-- reactive_ f = reactive \e s -> ((), f e s)
-- {-# INLINE reactive_ #-}

instance Functor m => Functor (ReactiveT m) where
  fmap f (ReactiveT g) = ReactiveT \e s -> fmap (fmap f) (g e s)
  {-# INLINE fmap #-}

instance Monad m => Applicative (ReactiveT m) where
  pure a = ReactiveT \_ s -> pure (s, a)
  {-# INLINE pure #-}
  (<*>) mf ma = ReactiveT \e s0 -> do
    (s1, f) <- unReactiveT mf e s0
    fmap (fmap f) $ unReactiveT ma e s1
  {-# INLINE (<*>) #-}

instance Monad m => Monad (ReactiveT m) where
  (>>=) ma mf = ReactiveT \e s0 -> do
    (s1, a) <- unReactiveT ma e s0
    unReactiveT (mf a) e s1
  {-# INLINE (>>=) #-}

instance Monad m => MonadReader ReactiveScope (ReactiveT m) where
  local f (ReactiveT g) = ReactiveT \e -> g (f e)
  {-# INLINE local #-}
  ask = ReactiveT \e s -> return (s, e)
  {-# INLINE ask #-}

instance Monad m => MonadState (ReactiveState m) (ReactiveT m) where
  state f = ReactiveT \_ s -> let (a, s') = f s in pure (s', a)
  {-# INLINE state #-}

instance MonadTrans ReactiveT where
  lift m = ReactiveT \_ s -> fmap (s,) m
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (ReactiveT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadFix m => MonadFix (ReactiveT m) where
  mfix f = ReactiveT \e s -> mfix \ ~(_, a) -> unReactiveT (f a) e s
  {-# INLINE mfix #-}

-- instance Monad m => MonadReactive (ReactiveT m) where
--   reactive f = ReactiveT \e s -> let (a, s') = f e s in pure (s', a)
--   {-# INLINE reactive #-}
