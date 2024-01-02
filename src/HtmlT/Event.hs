module HtmlT.Event where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Tuple
import Debug.Trace hiding (traceEventWith)
import GHC.Exts
import Unsafe.Coerce

import "this" HtmlT.RJS

-- | Contains a value that is subject to change over time. Provides
-- operations for reading the current value ('readDyn') and
-- subscribing to its future changes ('updates').
data Dynamic a = Dynamic
  { sample :: IO a
  -- ^ Read current dynamic. Use public alias 'readDyn' instead
  , updates :: Event a
  -- ^ Event that fires when the dynamic changes. Use public alias
  -- 'updates' instead
  }

-- | A mutable variable that allows for subscription to new values. It
-- shares a similar API to 'IORef' (see 'readRef', 'writeRef',
-- 'modifyRef')
data DynRef a = DynRef
  { dynamic :: Dynamic a
  -- ^ Holds the current dynamic and an event that notifies about dynamic
  -- modifications
  , modifier :: Modifier a
  -- ^ Funtion to update the dynamic
  }

-- | Function that updates the value inside the 'DynRef'
newtype Modifier a = Modifier
  { unModifier :: forall r. Bool -> (a -> (a, r)) -> RJS r
  -- ^ 'Bool' argument controls whether the modification should
  -- trigger an update event. It's possible to update the 'DynRef'
  -- without notifying the subscribers for optimization purposes, in
  -- cases when you know that all changes already been reflected in
  -- the DOM
  }

newtype Event a = Event { subscribe :: (a -> RJS ()) -> RJS () }

instance Functor Event where
  fmap f (Event s) = Event \k -> s . (. f) $ k

instance Functor Dynamic where
  fmap f (Dynamic s u) = Dynamic (fmap f s) (fmap f u)

instance Applicative Dynamic where
  pure = constDyn
  (<*>) = dynamicSplat

dynamicSplat :: Dynamic (a -> b) -> Dynamic a -> Dynamic b
dynamicSplat df da =
  let
    updates = Event \k -> mdo
      let
        fire newF newA = defer queueId do
          f <- liftIO $ maybe (readDyn df) pure newF
          a <- liftIO $ maybe (readDyn da) pure newA
          k (f a)
      df.updates.subscribe \f -> fire (Just f) Nothing
      da.updates.subscribe \a -> fire Nothing (Just a)
      queueId <- QueueId <$> state nextIntId
      return ()
    sample = liftA2 ($) df.sample da.sample
  in
    Dynamic {sample, updates}

unsafeTrigger :: EventId -> a -> RJS ()
unsafeTrigger eventId a = defer (QueueId eventId.unEventId) do
  callbacks <- gets $ fromMaybe [] .
    Map.lookup eventId . (.subscriptions)
  forM_ callbacks $ ($ unsafeCoerce @_ @Any a) . snd

-- | Defers a computation (typically an event firing) until the end of
-- the current reactive transaction. This allows for the avoidance of
-- double firing of events constructed from multiple other events.
defer :: QueueId -> RJS () -> RJS ()
defer k act = modify \s ->
  s {transaction_queue = Map.insert k act s.transaction_queue}

newEvent :: RJS (Event a, a -> RJS ())
newEvent = state \s0 ->
  let
    (eventId, s1) = nextIntId s0
    event = Event (reactive_ . unsafeSubscribe (EventId eventId))
    trig = unsafeTrigger (EventId eventId)
  in
    ((event, trig), s1)

newRef :: a -> RJS (DynRef a)
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
    dynamic = Dynamic (readIORef ioRef) event
  return DynRef {dynamic, modifier}

-- | Create a Dynamic that never changes its dynamic
constDyn :: a -> Dynamic a
constDyn a = Dynamic (pure a) never

-- | Event that will never fire
never :: Event a
never = Event \_ -> return ()

-- | Write new value into a 'DynRef'
--
-- > ref <- newRef "Initial dynamic"
-- > transactionWrite ref "New dynamic"
-- > readRef ref
-- "New dynamic"
writeRef :: DynRef a -> a -> RJS ()
writeRef ref a = modifyRef ref (const a)

-- | Read the current value held by given 'DynRef'
--
-- > ref <- newRef "Hello there!"
-- > readRef ref
-- "Hello there!"
readRef :: MonadIO m => DynRef a -> m a
readRef = readDyn . (.dynamic)

-- | Update a 'DynRef' by applying given function to the current value
--
-- > ref <- newRef [1..3]
-- > modifyRef ref $ fmap (*2)
-- [2, 4, 6]
modifyRef :: DynRef a -> (a -> a) -> RJS ()
modifyRef (DynRef _ (Modifier mod)) f = mod True $ (,()) . f

-- | Update a 'DynRef' with first field of the tuple and return back
-- the second field. The name is intended to be similar to
-- 'atomicModifyIORef' but there are no atomicity guarantees
-- whatsoever
atomicModifyRef :: DynRef a -> (a -> (a, r)) -> RJS r
atomicModifyRef (DynRef _ (Modifier mod)) f = mod True f

-- | Extract a 'Dynamic' out of 'DynRef'
fromRef :: DynRef a -> Dynamic a
fromRef = (.dynamic)

-- | Read the dynamic held by a 'Dynamic'
readDyn :: MonadIO m => Dynamic a -> m a
readDyn = liftIO . (.sample)

-- | Executes an action currently held inside the 'Dynamic' and every
-- time the dynamic changes.
performDyn :: Dynamic (RJS ()) -> RJS ()
performDyn d = do
  join $ liftIO d.sample
  d.updates.subscribe id

-- | Return a 'Dynamic' for which updates only fire when the value
-- actually changes according to Eq instance
holdUniqDyn :: Eq a => Dynamic a -> Dynamic a
holdUniqDyn = holdUniqDynBy (==)
{-# INLINE holdUniqDyn #-}

-- TODO: The name could be mesleading because it works differently
-- compare to the holdUniqDynBy function in reflex. Unlike the
-- original this version will perform comparisons equal to
-- @number_of_updates × number_of_subscriptions@ as opposed to once
-- per update in reflex (I could be wrong in my understanding of
-- 'holdUniqDynBy' in Reflex)
-- | Same as 'holdUniqDyn' but accepts arbitrary equality test
-- function
holdUniqDynBy :: (a -> a -> Bool) -> Dynamic a -> Dynamic a
holdUniqDynBy equalFn da =
  let
    updates = Event \k -> do
      old <- liftIO da.sample
      oldRef <- liftIO $ newIORef old
      da.updates.subscribe \new -> do
        old <- liftIO $ atomicModifyIORef' oldRef (new,)
        unless (old `equalFn` new) $ k new
  in
    Dynamic {sample = da.sample, updates}

-- | Produce a new Dynamic by applying a function to both the source
-- (Dynamic a) and previous value of itself
foldDynMaybe :: (a -> b -> Maybe b) -> b -> Dynamic a -> RJS (Dynamic b)
foldDynMaybe f initB dynA = do
  initA <- liftIO dynA.sample
  refB <- newRef $ fromMaybe initB $ f initA initB
  dynA.updates.subscribe \newA -> do
    oldB <- liftIO refB.dynamic.sample
    forM_ (f newA oldB) $ writeRef refB
  return refB.dynamic

-- | Alternative version if 'fmap' where given function will only be
-- called once every time 'Dynamic a' value changes, whereas in 'fmap'
-- it would be called once for each subscription per change event. As
-- a general guideline, if the function @f! is inexpensive, choose
-- @fmap f@. Otherwise, consider using @mapDyn f@.
mapDyn :: (a -> b) -> Dynamic a -> RJS (Dynamic b)
mapDyn fun da = do
  initialA <- liftIO $ da.sample
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef (fun initialA)
  queueId <- QueueId <$> state nextIntId
  let
    eventId = EventId queueId.unQueueId
    updates = Event (reactive_ . unsafeSubscribe eventId)
    fire = defer queueId do
      newB <- liftIO $ fun <$> readIORef latestA
      liftIO $ writeIORef latestB newB
      unsafeTrigger eventId newB
  da.updates.subscribe \newA -> do
    liftIO $ writeIORef latestA newA
    defer queueId fire
  return $ Dynamic (readIORef latestB) updates

mapDyn2 :: (a -> b -> c) -> Dynamic a -> Dynamic b -> RJS (Dynamic c)
mapDyn2 fun da db = do
  initialA <- liftIO $ da.sample
  initialB <- liftIO $ db.sample
  latestA <- liftIO $ newIORef initialA
  latestB <- liftIO $ newIORef initialB
  latestC <- liftIO $ newIORef (fun initialA initialB)
  queueId <- QueueId <$> state nextIntId
  let
    eventId = EventId queueId.unQueueId
    updates = Event (reactive_ . unsafeSubscribe eventId)
    fire = defer queueId do
      newC <- liftIO $ fun <$> (readIORef latestA) <*> (readIORef latestB)
      liftIO $ writeIORef latestC newC
      unsafeTrigger eventId newC
  da.updates.subscribe \newA -> do
    liftIO $ writeIORef latestA newA
    defer queueId fire
  db.updates.subscribe \newB -> do
    liftIO $ writeIORef latestB newB
    defer queueId fire
  return $ Dynamic (readIORef latestC) updates

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
  -> [Dynamic Any]
  -- ^ List of input Dynamics
  -> RJS (Dynamic a)
unsafeMapDynN fun dyns = do
  -- TODO: Try if list of IORefs is better than IORef of list
  initialInputs <- liftIO $ mapM (.sample) dyns
  initialOutput <- liftIO $ fun initialInputs
  latestInputsRef <- liftIO $ newIORef initialInputs
  latestOutputRef <- liftIO $ newIORef initialOutput
  queueId <- QueueId <$> state nextIntId
  let
    eventId = EventId queueId.unQueueId
    fire = defer queueId do
      newOutput <- liftIO $ fun =<< readIORef latestInputsRef
      liftIO $ writeIORef latestOutputRef newOutput
      unsafeTrigger eventId newOutput
    updates = Event (reactive_ . unsafeSubscribe eventId)
    updateList _ _ [] = []
    updateList 0 a (_:xs) = a:xs
    updateList n a (x:xs) = x : updateList (pred n) a xs
  forM_ (zip [0..] dyns) \(i::Int, adyn) -> do
    adyn.updates.subscribe \newVal -> do
      liftIO $ modifyIORef latestInputsRef $ updateList i newVal
      defer queueId fire
  return $ Dynamic (readIORef latestOutputRef) updates

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Apply a lens to the value inside 'DynRef'
lensMap :: forall s a. Lens' s a -> DynRef s -> DynRef a
lensMap l s =
  let
    dynamic = Dynamic
      (fmap (getConst . l Const) s.dynamic.sample)
      (fmap (getConst . l Const) s.dynamic.updates)
    modifier = Modifier \u f ->
      unModifier s.modifier u $ swap . l (swap . f)
  in
    DynRef {dynamic, modifier}

-- | Run a reactive transaction.
dynStep :: RJS a -> RJS a
dynStep act = loop0 act where
  loop0 :: RJS a -> RJS a
  loop0 act = do
    r <- act
    loop1 =<< gets (.transaction_queue)
    return r
  loop1 :: Map QueueId (RJS ()) -> RJS ()
  loop1 q =
    case Map.minViewWithKey q of
      Nothing -> return ()
      Just ((_, newAct), newQueue) -> do
        modify \s -> s {transaction_queue = newQueue}
        newAct
        loop1 =<< gets (.transaction_queue)

-------------------
-- DEBUG HELPERS --
-------------------

-- | Print a debug message each time given event fires
traceEvent :: Show a => String -> Event a -> Event a
traceEvent tag = traceEventWith (((tag <> ": ") <>) . show)

-- | Print a debug message when the event fires using given printing
-- function
traceEventWith :: (a -> String) -> Event a -> Event a
traceEventWith showA (Event f) =
  Event \c -> f (c . (\x -> trace (showA x) x))

-- | Print a debug message when value inside the Dynamic changes
traceDyn :: Show a => String -> Dynamic a -> Dynamic a
traceDyn tag = traceDynWith (((tag <> ": ") <>) . show)

-- | Print a debug message when value inside Dynamic changes using
-- given printing function
traceDynWith :: (a -> String) -> Dynamic a -> Dynamic a
traceDynWith showA dyn = dyn
  { updates = traceEventWith showA dyn.updates
  }

-- | Print a debug message when value inside the DynRef changes
traceRef :: Show a => String -> DynRef a -> DynRef a
traceRef tag ref = ref
  { dynamic = traceDynWith (((tag <> ": ") <>) . show) ref.dynamic
  }

-- | Print a debug message when value inside the DynRef changes
traceRefWith :: (a -> String) -> DynRef a -> DynRef a
traceRefWith f ref = ref
  { dynamic = traceDynWith f ref.dynamic
  }
