{-| Defines the 'JsMVar' type along with operations to manipulate it.
'JsMVar' is designed to be similar in spirit to the 'MVar' type from
the base package. Useful for asynchronous interactions with browser
APIs.
-}
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

-- | Blocking read operation on a `JsMVar`. If the `JsMVar` is empty,
-- the thread is blocked until the `JsMVar` is filled with `writeJsMVar`.
-- Only the last thread to call `takeJsMVar` on the same `JsMVar` will
-- receive the value, all others will be garbage collected.
takeJsMVar :: JsMVar a -> RJS a
takeJsMVar mvar = go where
  go = do
    mval <- liftIO $ atomicModifyIORef' mvar.mvar_value (Nothing,)
    case mval of
      Nothing -> yield mvar.callback_id *> go
      Just val -> pure val

-- | Performs a nonblocking write operation on a 'JsMVar'. This
-- operation will wake up the last thread blocked by 'takeJsMVar', if
-- any
writeJsMVar :: JsMVar a -> a -> RJS ()
writeJsMVar mvar val = do
  liftIO $ writeIORef mvar.mvar_value (Just val)
  evalExpr $ TriggerCallback mvar.callback_id NullE
  return ()
