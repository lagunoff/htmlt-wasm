{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedRecordDot #-}
module JSM where

import Control.Exception
import Control.Monad.Reader
import Unsafe.Coerce
import Data.IORef
import GHC.Exts

import "this" Protocol

newtype JSM a = JSM { unJSM :: ReaderT Context IO (Result a) }

data Context = Context
  { continuations :: IORef [(Expr -> IO (Result Any))]
  }

data Result a where
  Pure :: a -> Result a
  Sync :: UpCmd -> Result Expr
  FMap :: (a -> b) -> Result a -> Result b

data InterruptException = InterruptException UpCmd

instance Show InterruptException where
  show _ = "(InterruptException (error \"<redacted>\"))"

instance Exception InterruptException where

instance Functor JSM where
  fmap f (JSM (ReaderT a)) = JSM $ ReaderT \c -> fmap (FMap f) (a c)

instance Applicative JSM where
  pure = JSM . pure . Pure
  (<*>) mf ma = JSM $ ReaderT \c -> do
    let
      g :: forall a b. (a -> IO (Result b)) -> Result a -> IO (Result b)
      g cont = \case
        Pure a -> cont a
        Sync cmd -> do
          modifyIORef' c.continuations (unsafeCoerce cont:)
          throwIO $ InterruptException cmd
        FMap f i ->
          g (cont . f) i
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
        Sync cmd -> do
          modifyIORef' c.continuations (unsafeCoerce cont:)
          throwIO $ InterruptException cmd
        FMap f i ->
          g (cont . f) i
    g (flip runReaderT c . unJSM . mf) result

runTillInterruption :: Context -> JSM a -> IO (Either UpCmd a)
runTillInterruption c jsm = do
  result <- fmap Right (runReaderT (unJSM jsm) c)
    `catch` \(InterruptException upcmd) -> pure (Left upcmd)
  let
      g :: forall a b. (a -> IO b) -> Result a -> IO b
      g cont = \case
        Pure a -> cont a
        Sync cmd -> do
          modifyIORef' c.continuations (unsafeCoerce cont:)
          throwIO $ InterruptException cmd
        FMap f i ->
          g (cont . f) i
  case result of
    Left e -> return (Left e)
    Right r -> g (pure . Right) r

liftCMD :: UpCmd -> JSM Expr
liftCMD cmd = JSM $ ReaderT \_ -> return (Sync cmd)
