{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module JSM where

import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.ByteString
import GHC.Exts
import Unsafe.Coerce
import qualified Data.ByteString.Char8 as Char8

import "this" Protocol

newtype JSM a = JSM { unJSM :: ReaderT Context IO (Result a) }

data Context = Context
  { continuations :: IORef [(Expr -> IO (Result Any))]
  , dom_builder_id :: DomBuilderId
  }

data Result a where
  Pure :: a -> Result a
  Cmd :: UpCmd -> Result Expr
  Interrupt :: UpCmd -> (Expr -> IO (Result b)) -> Result b
  FMap :: (a -> b) -> Result a -> Result b

instance Functor JSM where
  fmap f (JSM (ReaderT a)) = JSM $ ReaderT \c -> fmap (FMap f) (a c)

instance Applicative JSM where
  pure = JSM . pure . Pure
  (<*>) mf ma = JSM $ ReaderT \c -> do
    let
      g :: forall a b. (a -> IO (Result b)) -> Result a -> IO (Result b)
      g cont = \case
        Pure a -> cont a
        Cmd cmd ->
          return (Interrupt cmd cont)
        FMap f i ->
          g (cont . f) i
        Interrupt cmd c2 ->
          return $ Interrupt cmd (g cont <=< c2)
    r1 <- runReaderT (unJSM mf) c
    g (\f -> do
         r2 <- runReaderT (unJSM ma) c
         g (\a -> pure (Pure (f a))) r2
      ) r1

instance Monad JSM where
  (>>=) ma mf = JSM $ ReaderT \c -> do
    result <- runReaderT (unJSM ma) c
    let
      g :: forall a b. (a -> IO (Result b)) -> Result a -> IO (Result b)
      g cont = \case
        Pure a -> cont a
        Cmd cmd ->
          return (Interrupt cmd cont)
        FMap f i ->
          g (cont . f) i
        Interrupt cmd c ->
          return $ Interrupt cmd (\e -> g cont =<< c e)
    g (flip runReaderT c . unJSM . mf) result

instance MonadReader Context JSM where
  ask = JSM $ ReaderT $ pure . Pure
  local f (JSM (ReaderT g)) = JSM $ ReaderT $ g . f

runTillInterruption :: forall a. Context -> JSM a -> IO (Either UpCmd a)
runTillInterruption c jsm = do
  result <- runReaderT (unJSM jsm) c `catch` \(e :: SomeException) ->
    -- UncaughtException command won't return a value from JS side,
    -- therefore we can unsafely coerce a ~ Expr
    pure (unsafeCoerce (Cmd (UncaughtException (Char8.pack (show e)))))
  let
    g :: forall a. Result a -> IO (Either UpCmd a)
    g = \case
      Pure a -> return (Right a)
      Cmd cmd -> return (Left cmd)
      FMap f i -> fmap (fmap f) (g i)
      Interrupt cmd cont -> do
        modifyIORef' c.continuations (unsafeCoerce cont :)
        return $ Left cmd
  g result

newScope :: JSM ScopeId
newScope = do
  resultExp <- liftCMD NewScope
  case resultExp of
    Num scopeId ->
      return (ScopeId scopeId)
    _ ->
      error "newScope: unexpected result expression"

freeScope :: ScopeId -> JSM ()
freeScope scopeId =
  void $ liftCMD $ FreeScope scopeId

withScope :: (ScopeId -> JSM a) -> JSM a
withScope f = do
  scopeId <- newScope
  result <- f scopeId
  result <$ freeScope scopeId

liftCMD :: UpCmd -> JSM Expr
liftCMD cmd = JSM $ ReaderT \_ -> return (Cmd cmd)

el :: ByteString -> [(ByteString, ByteString)] -> JSM a -> JSM a
el tagName attrs child = do
  domBuilderId <- asks (.dom_builder_id)
  let attrs' = fmap (\(k, v) -> (k, Str v)) attrs
  liftCMD (Eval (El domBuilderId tagName attrs'))
  result <- child
  liftCMD (Eval (PopDomBuilder domBuilderId))
  return result

text :: ByteString -> JSM ()
text contents = do
  domBuilderId <- asks (.dom_builder_id)
  liftCMD (Eval (Text domBuilderId contents))
  return ()
