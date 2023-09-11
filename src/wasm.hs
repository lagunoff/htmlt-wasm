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
  case downCmd of
    Start -> do
      result <- runTillInterruption jsmContext jsmMain
      case result of
        Left upCmd -> do
          storeByteString $ BSL.toStrict $ Binary.encode upCmd
        Right () -> do
          storeByteString $ BSL.toStrict $ Binary.encode Exit
    Return exp -> do
      tipCont <- atomicModifyIORef' continuationsRef \case
        [] -> ([], Nothing)
        x:xs -> (xs, Just x)
      case tipCont of
        Nothing -> storeByteString $ BSL.toStrict $ Binary.encode Exit
        Just c -> do
          result <- runTillInterruption jsmContext (JSM (ReaderT (const (c exp))))
          case result of
            Left upCmd -> do
              storeByteString $ BSL.toStrict $ Binary.encode upCmd
            Right _ -> do
              storeByteString $ BSL.toStrict $ Binary.encode Exit
  where
    jsmContext = JSM.Context continuationsRef

foreign export ccall hs_malloc :: Int -> IO (Ptr a)
hs_malloc = Alloc.callocBytes
foreign export ccall hs_free :: Ptr a -> IO ()
hs_free = Alloc.free

main = return ()

jsmMain :: JSM ()
jsmMain = do
  liftCMD (Eval (Call (Var "console") "log" [Obj [("Fuck!!", Str "Yeah!!")]]))
  liftCMD (Eval createH1)
  return ()
  where
    createH1 = Call (Var "document" `Dot` "body") "appendChild"
      [Call (Var "document") "createElement" [Str "H1"]]
