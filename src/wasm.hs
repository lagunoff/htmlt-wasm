{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
import Control.Monad.Reader
import Data.IORef
import Data.Word
import Foreign.Ptr
import GHC.Exts
import System.IO.Unsafe
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Foreign.Marshal.Alloc as Alloc

import "this" Protocol
import "this" JSM

foreign export ccall app :: Ptr Word8 -> IO (Ptr Word8)

continuationsRef :: IORef [(Expr -> IO (Result Any))]
continuationsRef = unsafePerformIO $ newIORef []

app :: Ptr Word8 -> IO (Ptr Word8)
app p = do
  downCmd <- Binary.decode . BSL.fromStrict <$> loadByteString p
  upCmd <- handleCommand downCmd
  storeByteString $ BSL.toStrict $ Binary.encode upCmd

handleCommand :: DownCmd -> IO UpCmd
handleCommand = \case
  Start -> do
    result <- runTillInterruption jsmContext jsmMain
    case result of
      Left upCmd -> return upCmd
      Right () -> return Exit
  Return exp -> do
    tipCont <- atomicModifyIORef' continuationsRef \case
      [] -> ([], Nothing)
      x:xs -> (xs, Just x)
    case tipCont of
      Nothing ->
        return $ UncaughtException "Protocol violation: continuation is missing"
      Just c -> do
        result <- runTillInterruption jsmContext (JSM (ReaderT (const (c exp))))
        case result of
          Left upCmd -> return upCmd
          Right _ -> return Exit
  where
    jsmContext = JSM.Context continuationsRef (DomBuilderId 0)


foreign export ccall hs_malloc :: Int -> IO (Ptr a)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()

jsmMain :: JSM ()
jsmMain = do
  domBuilderId <- asks (.dom_builder_id)
  el "div" [("className", "root-div")] do
    el "h1" [("className", "root-h1")] (pure ())
    el "button" [("className", "root-button")] (text "Click me!")
  return ()
