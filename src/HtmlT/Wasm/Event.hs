module HtmlT.Wasm.Event where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List qualified as List
import Data.IORef
import Data.Maybe
import Data.Set qualified as Set
import Unsafe.Coerce
import GHC.Exts

import "this" HtmlT.Wasm.Types

newtype Event a = Event { unEvent :: (a -> WASM ()) -> WASM () }

-- | Contains a value that is subject to change over time. Provides
-- operations for reading the current value ('readDyn') and
-- subscribing to its future changes ('updates').
data Dynamic a = Dynamic
  { dynamic_read :: IO a
  -- ^ Read current value. Use public alias 'readDyn' instead
  , dynamic_updates :: Event a
  -- ^ Event that fires when the value changes. Use public alias
  -- 'updates' instead
  }

-- | A mutable variable that allows for subscription to new values. It
-- shares a similar API to 'IORef' (see 'readRef', 'writeRef',
-- 'modifyRef')
data DynRef a = DynRef
  { dynref_value :: Dynamic a
  -- ^ Holds the current value and an event that notifies about value
  -- modifications
  , dynref_modifier :: Modifier a
  -- ^ Funtion to update the value
  }

-- | Function that updates the value inside the 'DynRef'
newtype Modifier a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> WASM r
  -- ^ 'Bool' argument controls whether the modification should
  -- trigger an update event. It's possible to update the 'DynRef'
  -- without notifying the subscribers for optimization purposes, in
  -- cases when you know that all changes already been reflected in
  -- the DOM
  }

unsafeSubscribe :: EventId -> (a -> WASM ()) -> WASMEnv -> WASMState -> WASMState
unsafeSubscribe eventId k e s0 =
  let
    (subId, s1) = nextQueueId s0
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
      ) . fromMaybe Map.empty) e.finalizer_ns s1.finalizers
  in
    s1 {subscriptions, finalizers}

unsafeTrigger :: EventId -> a -> WASM ()
unsafeTrigger eventId a = defer (unEventId eventId) do
  callbacks <- gets $ fromMaybe [] .
    Map.lookup eventId . (.subscriptions)
  forM_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

nextQueueId :: WASMState -> (QueueId, WASMState)
nextQueueId s =
  (s.id_supply, s {id_supply = succ s.id_supply})

-- | Defers a computation (typically an event firing) until the end of
-- the current reactive transaction. This allows for the avoidance of
-- double firing of events constructed from multiple other events.
defer :: QueueId -> WASM () -> WASM ()
defer k act = modify \s ->
  s {transaction_queue = Map.insert k act s.transaction_queue}

newEvent :: WASM (Event a, a -> WASM ())
newEvent = state \s0 ->
  let
    (eventId, s1) = nextQueueId s0
    event = Event (reactive_ . unsafeSubscribe (EventId eventId))
    trig = unsafeTrigger (EventId eventId)
  in
    ((event, trig), s1)

newRef :: a -> WASM (DynRef a)
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
constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event a
never = Event \_ -> return ()
-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial value"
-- > transactionWrite ref "New value"
-- > readRef ref
-- "New value"
writeRef :: DynRef a -> a -> WASM ()
writeRef ref a = modifyRef ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . dynref_value

-- | Update a 'DynRef' by applying given function to the current value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: DynRef a -> (a -> a) -> WASM ()
modifyRef (DynRef _ (Modifier mod)) f = mod True $ (,()) . f

-- | Update a 'DynRef' with first field of the tuple and return back
-- the second field. The name is intended to be similar to
-- 'atomicModifyIORef' but there are no atomicity guarantees
-- whatsoever
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> WASM r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = dynref_value

-- | Read the value held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . dynamic_read

-- | Extract the updates Event from a 'Dynamic'
updates :: Dynamic a -> Event a
updates = dynamic_updates

-- | Attach a listener to the event and return an action to detach the
-- listener
subscribe :: Event a -> (a -> WASM ()) -> WASM ()
subscribe (Event s) k = s k

-- | Executes an action currently held inside the 'Dynamic' and every
-- time the value changes.
performDyn :: Dynamic (WASM ()) -> WASM ()
performDyn d = do
  join $ liftIO $ dynamic_read d
  subscribe d.dynamic_updates id

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- TODO: holdUniqDynBy could be a misleading name, because it won't
-- hold the value for the whole Dynamic, instead it will perform the
-- comparison for each subscription
-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equalFn Dynamic{..} = Dynamic dynamic_read
  (Event \k -> do
    old <- liftIO dynamic_read
    oldRef <- liftIO (newIORef old)
    unEvent dynamic_updates \new -> do
      old <- liftIO $ atomicModifyIORef' oldRef (new,)
      unless (old `equalFn` new) $ k new
  )

type Subscriptions = Map EventId [(SubscriptionId, Any -> WASM ())]

type Finalizers = Map FinalizerNs (Map FinalizerKey FinalizerValue)

finalizeNamespace :: FinalizerNs -> WASM ()
finalizeNamespace ns = do
  removedList <- state \s ->
    let
      (removed, finalizers0) = Map.alterF (,Nothing) ns $ s.finalizers
      removedList = maybe [] Map.toList removed
      subscriptions = unsubscribe removedList s.subscriptions
      finalizers = removeFinalizer removedList finalizers0
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

    removeFinalizer :: [(FinalizerKey, FinalizerValue)] -> Finalizers -> Finalizers
    removeFinalizer [] !s = s
    removeFinalizer ((_, ParentNamespace p) : _) !s = -- Expecting at most one ParentNamespace
      Map.alter (fmap (Map.delete (FinalizerCustomId (unFinalizerNs p)))) p s
    removeFinalizer (_ : xs) !s = removeFinalizer xs s

    runCustomFinalizers :: [(FinalizerKey, FinalizerValue)] -> WASM ()
    runCustomFinalizers [] = return ()
    runCustomFinalizers ((_, CustomFinalizer w) : xs) = w *> runCustomFinalizers xs
    runCustomFinalizers ((_, NamespaceFinalizer n) : xs) = do
      finalizeNamespace n *> runCustomFinalizers xs
    runCustomFinalizers ((_, _) : xs) = runCustomFinalizers xs

    deleteSubs _ss [] = []
    deleteSubs ss ((s, c):xs)
      | Set.member s ss = xs
      | otherwise = (s, c) : deleteSubs ss xs

newNamespace :: WASM FinalizerNs
newNamespace = reactive \e s0 ->
  let
    (namespaceId, s1) = nextQueueId s0
    finalizerKey = FinalizerCustomId namespaceId
    namespace = FinalizerNs namespaceId
    finalizers = Map.alter
      (Just . Map.insert finalizerKey
        (NamespaceFinalizer namespace) . fromMaybe Map.empty
      ) e.finalizer_ns s1.finalizers
  in
    (namespace, s1 {finalizers})

-- | Run a reactive transaction.
dynStep :: WASM a -> WASM a
dynStep act = loop0 act where
  loop0 :: WASM a -> WASM a
  loop0 act = do
    r <- act
    loop1 =<< gets (.transaction_queue)
    return r
  loop1 :: Map QueueId (WASM ()) -> WASM ()
  loop1 q =
    case Map.minViewWithKey q of
      Nothing -> return ()
      Just ((_, newAct), newQueue) -> do
        modify \s -> s {transaction_queue = newQueue}
        newAct
        loop1 =<< gets (.transaction_queue)

instance Functor Event where
  fmap f (Event s) = Event \k -> s . (. f) $ k

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
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
        eventId <- state nextQueueId
        return ()
    in
      Dynamic
        { dynamic_read = liftA2 ($) (dynamic_read df) (dynamic_read da)
        , dynamic_updates = updatesEvent
        }
