module HtmlT.Wasm.DevServer where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.List qualified as List
import Data.Set qualified as Set
import Network.HTTP.Types as H
import Network.Wai as WAI
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Application.Static
import Foreign.Store

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Protocol

data DevServerOptions = DevServerOptions
  { server_state_ref :: IORef DevServerState
  , wasm_main_ref :: IORef (WA ())
  }

data DevServerState = DevServerState
  { connections :: Map ConnectionId ConnectionInfo
  , id_supply :: ConnectionId
  }

data ConnectionInfo = ConnectionInfo
  { connection :: Connection
  , options :: WasmOptions
  }

newtype ConnectionId = ConnectionId {unConnectionId :: Int}
  deriving newtype (Ord, Eq, Num, Enum)

runDevServer :: WA () -> IO ()
runDevServer wasmMain = do
  let
    static = staticApp (defaultFileServerSettings "./public")
    waiSettings = setPort 8081 Warp.defaultSettings
    storeId = 183
  devopts <- newDevServerOptions wasmMain
  lookupStore storeId >>= \case
    Nothing -> do
      writeStore (Store storeId) devopts
      void $ forkIO $ runSettings waiSettings $ devserverMiddleware devopts static
    Just store -> do
      opts :: DevServerOptions <- readStore store
      writeIORef opts.wasm_main_ref wasmMain
      serverState <- readIORef opts.server_state_ref
      forM_ serverState.connections \connInfo ->
        sendDataMessage connInfo.connection . Binary $ Binary.encode HotReload

devserverApplication :: DevServerOptions -> Application
devserverApplication opt =
  devserverMiddleware opt $ const
    ($ responseLBS status404 [] "Not found")

devserverMiddleware :: DevServerOptions -> Middleware
devserverMiddleware opts next req resp =
  case pathInfo req of
    [] -> indexHtmlApp req resp
    ["index.html"] -> indexHtmlApp req resp
    ["dev-server.sock"] -> devserverApp req resp
    _ -> next req resp
  where
    devserverApp =
      websocketsOr defaultConnectionOptions (devserverWebsocket opts) notFound
    notFound _ resp =
      resp $ responseLBS status404 [] "Not found"
    indexHtmlApp req resp = do
      let origin = inferOrigin req
      resp $ responseLBS status200
        [(hContentType, "text/html; charset=utf-8")] $ indexHtml origin
    indexHtml (BSL.fromStrict -> origin) = "\
      \<html>\n\
      \ <body>\n\
      \  <script src=\"/index.bundle.js\"></script>\n\
      \  <script>\n\
      \    startDevClient(\"" <> origin <> "/dev-server.sock\");\n\
      \  </script>\n\
      \ </body>\n\
      \</html>\n\
      \"
    inferOrigin req = WAI.requestHeaders req
      & List.lookup "Host"
      & fromMaybe "localhost"
      & ((if WAI.isSecure req then "wss://" else "ws://") <>)

devserverWebsocket :: DevServerOptions -> ServerApp
devserverWebsocket opt p =
  bracket acceptConn dropConn \(conn, _, options) ->
    withPingThread conn 30 (pure ()) $
      loop conn options
  where
    acceptConn = do
      connection <- acceptRequest p
      options <- newWasmOptions
      let connInfo = ConnectionInfo {options, connection}
      connId <- atomicModifyIORef' opt.server_state_ref \s ->
        ( s
          { id_supply = succ s.id_supply
          , connections = Map.insert s.id_supply connInfo s.connections
          }
        , s.id_supply
        )
      return (connection, connId, options)
    dropConn (_, connId, _) =
      modifyIORef' opt.server_state_ref \s -> s
        {connections = Map.delete connId s.connections}
    newWasmOptions = do
      wasm_state_ref <- newIORef WAState
        { var_storage = Set.fromList [0, 1]
        , evaluation_queue = []
        , subscriptions = Map.empty
        , finalizers = Map.empty
        , id_supply = 0
        , transaction_queue = Map.empty
        }
      continuations_ref <- newIORef []
      return WasmOptions {continuations_ref, wasm_state_ref}
    loop conn options  =
      try (receiveData conn) >>= \case
        Right (websocketBytes::ByteString) -> do
          let downCmd = Binary.decode . BSL.fromStrict $ websocketBytes
          wasmMain <- readIORef opt.wasm_main_ref
          upCmd <- handleCommand options wasmMain downCmd
          sendDataMessage conn . Binary $ Binary.encode upCmd
          loop conn options
        Left (_::ConnectionException) ->
          return ()

newDevServerOptions :: WA () -> IO DevServerOptions
newDevServerOptions wasmMain = do
  wasm_main_ref <- newIORef wasmMain
  server_state_ref <- newIORef $ DevServerState Map.empty 0
  return DevServerOptions {server_state_ref, wasm_main_ref}
