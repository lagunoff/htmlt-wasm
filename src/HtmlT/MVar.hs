module HtmlT.MVar where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef


import "this" HtmlT.Protocol
import "this" HtmlT.RJS


data JsMVar a = JsMVar
  { callback_id :: CallbackId
  , mvar_value :: IORef (Maybe a)
  }

newEmptyJsMVar :: RJS (JsMVar a)
newEmptyJsMVar = do
  callback_id <- CallbackId . unQueueId <$> state nextQueueId
  mvar_value <- liftIO $ newIORef Nothing
  return JsMVar {callback_id, mvar_value}

takeJsMVar :: JsMVar a -> RJS a
takeJsMVar mvar = go where
  go = do
    mval <- liftIO $ atomicModifyIORef' mvar.mvar_value (Nothing,)
    case mval of
      Nothing -> yield mvar.callback_id *> go
      Just val -> pure val

putJsMVar :: JsMVar a -> a -> RJS ()
putJsMVar mvar val = do
  liftIO $ writeIORef mvar.mvar_value (Just val)
  evalExpr $ AsyncCallback mvar.callback_id Null
  return ()
