module HtmlT.JS where

import Control.Monad
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

import "this" HtmlT.Protocol

-- | A computation capable of interacting with JavaScript.
newtype JS a = JS
  { unJS :: JSState -> IO (JSState, JSResult a)
  }

data JSResult a where
  PureResult :: a -> JSResult a
  EvalResult :: Expr -> JSResult JValue
  InterruptResult :: Expr -> (JValue -> JS b) -> JSResult b
  FMapResult :: (a -> b) -> JSResult a -> JSResult b

data JSState = JSState
  { var_storage :: Set VarId
  , evaluation_queue :: [Expr]
  , callbacks :: Map CallbackId (JValue -> JS Any)
  , id_supply :: QueueId
  -- ^ Source of unique identifiers for EventId, SubscriptionId and
  -- VarId (potentially can lead to clashes if it overflows in a
  -- long-living application, TODO: is this a legitimate concern?)
  } deriving Generic

emptyJSState :: JSState
emptyJSState = JSState
  { var_storage = Set.empty
  , callbacks = Map.empty
  , evaluation_queue = []
  , id_supply = 0
  }

newtype QueueId = QueueId { unQueueId :: Int64 }
  deriving newtype (Eq, Ord, Show, Num, Enum)

nextQueueId :: JSState -> (QueueId, JSState)
nextQueueId s =
  (s.id_supply, s {id_supply = succ s.id_supply})

newCallback :: (JValue -> JS Any) -> JSState -> (CallbackId, JSState)
newCallback k s =
  let
    callbackId = CallbackId $ unQueueId s.id_supply
    callbacks = Map.insert callbackId k s.callbacks
  in
    (callbackId, s {id_supply = succ s.id_supply, callbacks})

releaseCallback :: CallbackId -> JSState -> JSState
releaseCallback callbackId s =
  s {callbacks = Map.delete callbackId s.callbacks}

newVar :: JS VarId
newVar = state \s0 ->
  let
    (newQueueId, s1) = nextQueueId s0
    newVarId = VarId (unQueueId newQueueId)
    var_storage = Set.insert newVarId s1.var_storage
  in
    (newVarId, s1 {var_storage})

freeVar :: VarId -> JS ()
freeVar varId = do
  modify \s -> s { var_storage = Set.delete varId s.var_storage}
  enqueueExpr $ FreeVar varId

evalExpr :: Expr -> JS JValue
evalExpr e = JS \s -> return (s, EvalResult e)

enqueueExpr :: Expr -> JS ()
enqueueExpr e = modify \s ->
  s {evaluation_queue = e : s.evaluation_queue}

enqueueIfAlive :: VarId -> Expr -> JS ()
enqueueIfAlive varId e = modify \s ->
  let
    evaluation_queue =
      if Set.member varId s.var_storage
        then e : s.evaluation_queue else s.evaluation_queue
  in
    s {evaluation_queue}

flushQueue :: JS ()
flushQueue = do
  queue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  void $ evalExpr $ RevSeq queue

bindJsResult
  :: forall a b. JSResult a
  -> (a -> JS b)
  -> JSState
  -> IO (JSState, JSResult b)
bindJsResult r cont s = case r of
  PureResult a -> unJS (cont a) s
  EvalResult cmd ->
    return (s, InterruptResult cmd cont)
  FMapResult f i ->
    bindJsResult i (cont . f) s
  InterruptResult cmd c2 -> do
    let
      cont' exp = JS \s -> do
        (s', r') <- unJS (c2 exp) s
        bindJsResult r' cont s'
    return (s, InterruptResult cmd cont')

instance Functor JS where
  fmap f (JS g) = JS \s -> fmap h (g s)
    where
      h (s, PureResult a) = (s, PureResult (f a))
      h (s, r) = (s, FMapResult f r)
  {-# INLINE fmap #-}

instance Applicative JS where
  pure a = JS \s -> return (s, PureResult a)
  {-# INLINE pure #-}
  (<*>) mf ma = JS \s -> do
    (s, r) <- unJS mf s
    bindJsResult r (flip fmap ma) s
  {-# INLINE (<*>) #-}

instance Monad JS where
  (>>=) ma mf = JS \s -> do
    (s, r) <- unJS ma s
    bindJsResult r mf s
  {-# INLINE (>>=) #-}

instance MonadState JSState JS where
  state f = JS \s -> let (a, s') = f s in return (s', PureResult a)
  {-# INLINE state #-}

instance MonadIO JS where
  liftIO io = JS \s -> fmap ((s,) . PureResult) io
  {-# INLINE liftIO #-}

instance MonadFix JS where
  mfix f = JS \s -> mfix \ ~(_, a) -> unJS (f (extractPure a)) s
    where
      extractPure (PureResult a) = a
      extractPure _ = error
        "JavaScript commands in conjunction with MonadFix not supported"
  {-# INLINE mfix #-}
