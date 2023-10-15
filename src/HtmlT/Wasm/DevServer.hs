module HtmlT.Wasm.DevServer where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.Typeable
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Foreign.Store
import GHC.IO.Exception
import Network.HTTP.Types as H
import Network.Wai as WAI
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import System.IO

import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Protocol

data DevServerInstance = DevServerInstance
  { conn_state_ref :: IORef ConnectionState
  , app_state_ref :: IORef RunningApp
  }

data ConnectionState = ConnectionState
  { connections :: Map ConnectionId ConnectionInfo
  , id_supply :: ConnectionId
  }

data ConnectionInfo = ConnectionInfo
  { connection :: Connection
  , options :: WasmInstance
  }

data DebugConfig a = DebugConfig
  { open_resource :: IO a
  , close_resource :: a -> IO ()
  , init_app :: a -> IO (WA (), Application)
  }

data RunningApp = forall a. Typeable a => RunningApp
  { resource :: a
  , debug_config :: DebugConfig a
  , client_app :: WA ()
  , server_app :: Application
  }

newtype ConnectionId = ConnectionId {unConnectionId :: Int}
  deriving newtype (Ord, Eq, Num, Enum)

runDebug :: Typeable a => Warp.Settings -> DebugConfig a -> IO ()
runDebug settings debugCfg = do
  let storeId = 183
  hSetBuffering stderr LineBuffering
  lookupStore storeId >>= \case
    Nothing -> do
      devInst <- newInstance debugCfg
      writeStore (Store storeId) devInst
      let
        useCurrentApp req resp = do
          reloadState <- readIORef devInst.app_state_ref
          reloadState.server_app req resp
      void $ forkIO $ tryPort settings $
        devserverMiddleware devInst useCurrentApp
    Just store -> do
      oldInst <- readStore store
      updateInstance debugCfg oldInst
      connState <- readIORef oldInst.conn_state_ref
      forM_ connState.connections \connInfo ->
        sendDataMessage connInfo.connection . Binary $ Binary.encode HotReload
  where
    tryPort :: Warp.Settings -> Application -> IO ()
    tryPort settings application = do
      hPutStrLn stderr $ "Running a Dev Server at http://localhost:" <>
        show (getPort settings) <> "/"
      result <- try $ runSettings settings application
      case result of
        Right () -> return ()
        Left (e::IOException)
          | ioe_type e == ResourceBusy -> do
            hPutStrLn stderr $ "Already in use, trying next…"
            tryPort (setPort (getPort settings + 1) settings) application
          | otherwise -> throwIO e

runDebugDefault :: Warp.Port -> WA () -> IO ()
runDebugDefault port wasmApp =
  runDebug (Warp.setPort port Warp.defaultSettings) DebugConfig
    { open_resource = pure ()
    , close_resource = const (pure ())
    , init_app = const $ pure (wasmApp, notFound)
    }

devserverApplication :: DevServerInstance -> Application
devserverApplication opt =
  devserverMiddleware opt $ const
    ($ responseLBS status404 [] "Not found")

devserverMiddleware :: DevServerInstance -> Middleware
devserverMiddleware opts next req resp =
  case pathInfo req of
    [] -> indexHtmlApp req resp
    ["index.html"] -> indexHtmlApp req resp
    ["dev-server.sock"] -> devserverApp req resp
    _ -> next req resp
  where
    devserverApp =
      websocketsOr defaultConnectionOptions (devserverWebsocket opts) notFound
    indexHtmlApp req resp = do
      let origin = inferOrigin req
      resp $ responseLBS status200
        [(hContentType, "text/html; charset=utf-8")] $ indexHtml origin
    indexHtml (BSL.fromStrict -> origin) = "\
      \<html>\n\
      \ <body>\n\
      \  <script>\n\
      \    " <> BSL.fromStrict indexBundleJs <> "\n\
      \    startDevClient(\"" <> origin <> "/dev-server.sock\");\n\
      \  </script>\n\
      \ </body>\n\
      \</html>\n\
      \"
    inferOrigin req = WAI.requestHeaders req
      & List.lookup "Host"
      & fromMaybe "localhost"
      & ((if WAI.isSecure req then "wss://" else "ws://") <>)

notFound :: Application
notFound _ resp =
  resp $ responseLBS status404 [] "Not found"

devserverWebsocket :: DevServerInstance -> ServerApp
devserverWebsocket opt p =
  bracket acceptConn dropConn \(conn, _, options) ->
    withPingThread conn 30 (pure ()) $
      loop conn options
  where
    acceptConn = do
      connection <- acceptRequest p
      options <- newWasmInstance
      let connInfo = ConnectionInfo {options, connection}
      connId <- atomicModifyIORef' opt.conn_state_ref \s ->
        ( s
          { id_supply = succ s.id_supply
          , connections = Map.insert s.id_supply connInfo s.connections
          }
        , s.id_supply
        )
      return (connection, connId, options)
    dropConn (_, connId, _) =
      modifyIORef' opt.conn_state_ref \s -> s
        {connections = Map.delete connId s.connections}
    newWasmInstance = do
      wasm_state_ref <- newIORef emptyWAState
      continuations_ref <- newIORef []
      return WasmInstance {continuations_ref, wasm_state_ref}
    loop conn options  =
      try (receiveData conn) >>= \case
        Right (websocketBytes::ByteString) -> do
          let downCmd = Binary.decode . BSL.fromStrict $ websocketBytes
          runningApp <- readIORef opt.app_state_ref
          upCmd <- handleCommand options runningApp.client_app downCmd
          sendDataMessage conn . Binary $ Binary.encode upCmd
          loop conn options
        Left (_::ConnectionException) ->
          return ()

newInstance :: Typeable a => DebugConfig a -> IO DevServerInstance
newInstance debugCfg = do
  resource <- debugCfg.open_resource
  (client_app, server_app) <- debugCfg.init_app resource
  app_state_ref <- newIORef RunningApp
    { resource
    , debug_config = debugCfg
    , client_app
    , server_app
    }
  conn_state_ref <- newIORef $ ConnectionState Map.empty 0
  return DevServerInstance {conn_state_ref, app_state_ref}

updateInstance :: Typeable a => DebugConfig a -> DevServerInstance -> IO ()
updateInstance debugCfg devInst = do
  oldApp <- readIORef devInst.app_state_ref
  let tryOld = tryOldResource debugCfg oldApp
  case tryOld of
    Right oldResource -> do
      (client_app, server_app) <- debugCfg.init_app oldResource
      writeIORef devInst.app_state_ref RunningApp
        { resource = oldResource
        , debug_config = debugCfg
        , client_app
        , server_app
        }
    Left closeOld -> do
      closeOld
      newResource <- debugCfg.open_resource
      (client_app, server_app) <- debugCfg.init_app newResource
      writeIORef devInst.app_state_ref RunningApp
        { resource = newResource
        , debug_config = debugCfg
        , client_app
        , server_app
        }
  where
    tryOldResource :: forall a. Typeable a => DebugConfig a -> RunningApp -> Either (IO ()) a
    tryOldResource _ RunningApp {resource, debug_config}
      | Just Refl <- eqT0 @a resource = Right resource
      | otherwise = Left (debug_config.close_resource resource)

    eqT0 :: forall a b. (Typeable a, Typeable b) => b -> Maybe (a :~: b)
    eqT0 _ = eqT @a @b

-- | Run @yarn run webpack --mode production@ and copy contents from
-- @./dist-newstyle/index.bundle.js@ to update the 'indexBundleJs'
indexBundleJs :: ByteString
indexBundleJs = "(()=>{\"use strict\";var e=function(t,r){return e=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])},e(t,r)};function t(t,r){if(\"function\"!=typeof r&&null!==r)throw new TypeError(\"Class extends value \"+String(r)+\" is not a constructor or null\");function n(){this.constructor=t}e(t,r),t.prototype=null===r?Object.create(r):(n.prototype=r.prototype,new n)}function r(e,t,r,n){return new(r||(r=Promise))((function(i,s){function a(e){try{l(n.next(e))}catch(e){s(e)}}function o(e){try{l(n.throw(e))}catch(e){s(e)}}function l(e){var t;e.done?i(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(a,o)}l((n=n.apply(e,t||[])).next())}))}function n(e,t){var r,n,i,s,a={label:0,sent:function(){if(1&i[0])throw i[1];return i[1]},trys:[],ops:[]};return s={next:o(0),throw:o(1),return:o(2)},\"function\"==typeof Symbol&&(s[Symbol.iterator]=function(){return this}),s;function o(o){return function(l){return function(o){if(r)throw new TypeError(\"Generator is already executing.\");for(;s&&(s=0,o[0]&&(a=0)),a;)try{if(r=1,n&&(i=2&o[0]?n.return:o[0]?n.throw||((i=n.return)&&i.call(n),0):n.next)&&!(i=i.call(n,o[1])).done)return i;switch(n=0,i&&(o=[2&o[0],i.value]),o[0]){case 0:case 1:i=o;break;case 4:return a.label++,{value:o[1],done:!1};case 5:a.label++,n=o[1],o=[0];continue;case 7:o=a.ops.pop(),a.trys.pop();continue;default:if(!((i=(i=a.trys).length>0&&i[i.length-1])||6!==o[0]&&2!==o[0])){a=0;continue}if(3===o[0]&&(!i||o[1]>i[0]&&o[1]<i[3])){a.label=o[1];break}if(6===o[0]&&a.label<i[1]){a.label=i[1],i=o;break}if(i&&a.label<i[2]){a.label=i[2],a.ops.push(o);break}i[2]&&a.ops.pop(),a.trys.pop();continue}o=t.call(e,a)}catch(e){o=[6,e],n=0}finally{r=i=0}if(5&o[0])throw o[1];return{value:o[0]?o[1]:void 0,done:!0}}([o,l])}}}Object.create,Object.create,\"function\"==typeof SuppressedError&&SuppressedError;class i{static read_bytes(e,t){let r=new i;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){let n=[];for(let s=0;s<r;s++)n.push(i.read_bytes(e,t+8*s));return n}}class s{static read_bytes(e,t){let r=new s;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){let n=[];for(let i=0;i<r;i++)n.push(s.read_bytes(e,t+8*i));return n}}class a{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class o{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}class l{fd_advise(e,t,r){return-1}fd_allocate(e,t){return-1}fd_close(){return 0}fd_datasync(){return-1}fd_fdstat_get(){return{ret:-1,fdstat:null}}fd_fdstat_set_flags(e){return-1}fd_fdstat_set_rights(e,t){return-1}fd_filestat_get(){return{ret:-1,filestat:null}}fd_filestat_set_size(e){return-1}fd_filestat_set_times(e,t,r){return-1}fd_pread(e,t,r){return{ret:-1,nread:0}}fd_prestat_get(){return{ret:-1,prestat:null}}fd_prestat_dir_name(e,t){return{ret:-1,prestat_dir_name:null}}fd_pwrite(e,t,r){return{ret:-1,nwritten:0}}fd_read(e,t){return{ret:-1,nread:0}}fd_readdir_single(e){return{ret:-1,dirent:null}}fd_seek(e,t){return{ret:-1,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:-1,offset:0n}}fd_write(e,t){return{ret:-1,nwritten:0}}path_create_directory(e){return-1}path_filestat_get(e,t){return{ret:-1,filestat:null}}path_filestat_set_times(e,t,r,n,i){return-1}path_link(e,t,r,n){return-1}path_open(e,t,r,n,i,s){return{ret:-1,fd_obj:null}}path_readlink(e){return{ret:-1,data:null}}path_remove_directory(e){return-1}path_rename(e,t,r){return-1}path_symlink(e,t){return-1}path_unlink_file(e){return-1}}class u extends l{fd_fdstat_get(){return{ret:0,fdstat:new a(4,0)}}fd_read(e,t){let r=0;for(let n of t){if(!(this.file_pos<this.file.data.byteLength))break;{let t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_write(e,t){let r=0;if(this.file.readonly)return{ret:8,nwritten:r};for(let n of t){let t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){let e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class f{open(e){let t=new u(this);return 1&e&&t.fd_seek(0n,2),t}get size(){return BigInt(this.data.byteLength)}stat(){return new o(4,this.size)}truncate(){return this.readonly?63:(this.data=new Uint8Array([]),0)}constructor(e,t){this.data=new Uint8Array(e),this.readonly=!!t?.readonly}}function c(e){throw new Error(\"absurd: unreachable code\")}var d=function(){function e(){}return e.prototype.encode=function(e){var t=x(this,e),r=new Uint8Array(t);return U(this,r,0,e),r},e.prototype.decode=function(e){var t=A(this,e,0),r=t[0];return t[1],r},e}(),_=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),p=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),h=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),m=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),y=function(e){function r(t){var r=e.call(this)||this;return r._element=t,r}return t(r,e),r}(d),g=function(e){function r(t){var r=e.call(this)||this;return r._description=t,r}return t(r,e),r}(d),w=function(e){function r(t){var r=e.call(this)||this;return r._alternatives=t,r}return t(r,e),r}(d),b=function(e){function r(t){var r=e.call(this)||this;return r._self=t,r}return t(r,e),r}(d),v=function(e){function r(t){var r=e.call(this)||this;return r._tuple=t,r}return t(r,e),r}(d);function x(e,t){if(e instanceof _)return 1;if(e instanceof p)return 8;if(e instanceof m){var r=t;return(n=8)+(new TextEncoder).encode(r).length}if(e instanceof h)return(n=8)+t.length;if(e instanceof y){var n=8;return t.reduce((function(t,r){return t+x(e._element,r)}),n)}if(e instanceof g){var i=t;return Object.keys(e._description).reduce((function(t,r){return t+x(e._description[r],i[r])}),0)}if(e instanceof w){var s=t;return E(Object.keys(e._alternatives).length)+x(e._alternatives[s.tag],s)}if(e instanceof b)return x(e._self,t);if(e instanceof v){var a=t;return e._tuple.reduce((function(e,t,r){return e+x(t,a[r])}),0)}return c()}function A(e,t,r){if(e instanceof _)return[t[r],r+1];if(e instanceof p)return[u=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),r+8];if(e instanceof m){var n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),i=t.subarray(r+8,r+8+n);return[new TextDecoder(\"utf8\").decode(i),r+8+n]}if(e instanceof h)return n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),[t.subarray(r+8,r+8+n),r+8+n];if(e instanceof y){n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);for(var s=[],a=r+8,o=0;o<n;o++){var l=A(e._element,t,a),u=l[0],f=l[1];s.push(u),a=f}return[s,a]}if(e instanceof g){var d=r,x=Object.fromEntries(Object.entries(e._description).map((function(e){var r=e[0],n=A(e[1],t,d),i=n[0],s=n[1];return d=s,[r,i]})));return[x,d]}if(e instanceof w){var U=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(E(Object.keys(e._alternatives).length),t,r),k=U[0],B=U[1],C=A(e._alternatives[k],t,B),I=C[0],D=C[1];return I.tag=k,[I,D]}if(e instanceof b)return A(e._self,t,r);if(e instanceof v){var N=r,S=e._tuple.map((function(e){var r=A(e,t,N),n=r[0],i=r[1];return N=i,n}));return[S,N]}return c()}function U(e,t,r,n){if(e instanceof _)return t[r]=n,r+1;if(e instanceof p){var i=n;return t[r+7]=255&i,t[r+6]=i>>8&255,t[r+5]=i>>16&255,t[r+4]=i>>24&255,r+8}if(e instanceof m){var s=n,a=(new TextEncoder).encode(s),o=a.length;return t[r+7]=255&o,t[r+6]=o>>8&255,t[r+5]=o>>16&255,t[r+4]=o>>24&255,t.set(a,r+8),r+8+o}if(e instanceof h){var l=n;return o=l.length,t[r+7]=255&o,t[r+6]=o>>8&255,t[r+5]=o>>16&255,t[r+4]=o>>24&255,t.set(l,r+8),r+8+o}if(e instanceof y){var u=n;o=u.length,t[r+7]=255&o,t[r+6]=o>>8&255,t[r+5]=o>>16&255,t[r+4]=o>>24&255;for(var f=r+8,d=0;d<o;d++)f=U(e._element,t,f,u[d]);return f}if(e instanceof g){var x=n;for(var A in f=r,e._description)Object.prototype.hasOwnProperty.call(e._description,A)&&(f=U(e._description[A],t,f,x[A]));return f}if(e instanceof w){var k=n.tag,B=E(Object.keys(e._alternatives).length);return t[r]=k,U(e._alternatives[k],t,r+B,n)}if(e instanceof b)return U(e._self,t,r,n);if(e instanceof v){var C=n,I=r;return e._tuple.forEach((function(e,r){I=U(e,t,I,C[r])})),I}return c()}function E(e){return Math.ceil(Math.log2(e)/8)}var k,B,C,I=new _,D=new p,N=new m;function S(e){return new y(e)}function V(e){return new g(e)}function T(e){return new w(e)}function O(){for(var e=[],t=0;t<arguments.length;t++)e[t]=arguments[t];return new v(e)}function j(e){var t=new b(void 0),r=e(t);return t._self=r,r}function J(e){return e[0]}function L(e){return e[1]}function R(e,t,r,n){switch(n.tag){case z.Null:return null;case z.Boolean:return 0!=n[0];case z.Num:case z.Str:return n[0];case z.Arr:return n[0].map(R.bind(void 0,e,t,r));case z.Obj:return Object.fromEntries(n[0].map((function(n){var i=n[0],s=n[1];return[i,R(e,t,r,s)]})));case z.Dot:return(u=R(e,t,r,n[0]))[n[1]];case z.AssignProp:var i=R(e,t,r,n[2]);return R(e,t,r,n[0])[n[1]]=i,i;case z.Ix:return(i=R(e,t,r,n.exp))[n.ix];case z.Add:return(u=R(e,t,r,n[0]))+R(e,t,r,n[1]);case z.Subtract:return(u=R(e,t,r,n[0]))-R(e,t,r,n[1]);case z.Multiply:return(u=R(e,t,r,n[0]))*R(e,t,r,n[1]);case z.Divide:return(u=R(e,t,r,n[0]))/R(e,t,r,n[1]);case z.Id:for(var s=n[0],a=e;a;a=L(a)){var o=J(a);if(s in o)return o[s]}throw new Error(\"Variable not in scope: \"+n[0]);case z.Lam:return function(){return R(e,[arguments,t],r,n.body)};case z.Arg:a=t;for(var l=0;a;){if(l==n.scopeIx)return J(a)[l];a=L(a),l++}throw new Error(\"Argument scope out of a rabge: \"+n.scopeIx);case z.Apply:return(u=R(e,t,r,n[0])).apply(void 0,n[1].map(R.bind(void 0,e,t,r)));case z.Call:var u;return(u=R(e,t,r,n[0]))[n[1]].apply(u,n[2].map(R.bind(void 0,e,t,r)));case z.AssignVar:return i=R(e,t,r,n.rhs),K.set(n.lhs,i),i;case z.FreeVar:return K.delete(n.varId);case z.Var:return K.get(n.varId);case z.InsertNode:var f=R(e,t,r,n.parent),d=R(e,t,r,n.child);return G.insertIntoBuilder(f,d),null;case z.WithBuilder:var _=R(e,t,r,n.builder);return R(e,t,r,n.builderContent)(_),_;case z.CreateElement:return document.createElement(n.tagName);case z.CreateText:return document.createTextNode(n.content);case z.ElementProp:var p=R(e,t,r,n.node),h=R(e,t,r,n.propValue);return G.assignProperty(p,n.propName,h),null;case z.ElementAttr:var m=R(e,t,r,n.node);return G.assignAttribute(m,n.attrName,n.attrValue),null;case z.AddEventListener:var y=R(e,t,r,n.node),g=R(e,t,r,n.listener);return G.addEventListener(y,n.eventName,g),null;case z.ToggleClass:var w=R(e,t,r,n.node);return G.toggleClass(w,n.className,Boolean(n.enable)),null;case z.AssignText:return R(e,t,r,n.node).textContent=n.content,null;case z.InsertBoundary:var b=R(e,t,r,n.parent);return G.insertBoundary(b);case z.ClearBoundary:var v=R(e,t,r,n.boundary);return G.clearBoundary(v,Boolean(n.detach));case z.RevSeq:return n.exprs.reduceRight((function(n,i){return R(e,t,r,i)}),null);case z.ExecCallback:var x=R(e,t,r,n.arg);return r({tag:H.ExecCallback,arg:P(x),callbackId:n.callbackId});case z.UncaughtException:throw new Error(n.message)}c()}function P(e){if(\"boolean\"==typeof e)return{tag:C.JBool,0:e?1:0};if(\"number\"==typeof e)return{tag:C.JNum,0:e};if(\"string\"==typeof e)return{tag:C.JStr,0:e};if(Array.isArray(e))return{tag:C.JArr,0:e.map(P)};if(null==e)return{tag:C.JNull};var t=Object.entries(e).map((function(e){return[e[0],P(e[1])]}));return{tag:C.JObj,0:t}}new h,function(e){e[e.JNull=0]=\"JNull\",e[e.JBool=1]=\"JBool\",e[e.JNum=2]=\"JNum\",e[e.JStr=3]=\"JStr\",e[e.JArr=4]=\"JArr\",e[e.JObj=5]=\"JObj\"}(C||(C={}));var z,M=j((function(e){var t;return T(((t={})[C.JNull]=V({}),t[C.JBool]=V({0:I}),t[C.JNum]=V({0:D}),t[C.JStr]=V({0:N}),t[C.JArr]=V({0:S(e)}),t[C.JObj]=V({0:S(O(N,e))}),t))}));!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.Num=2]=\"Num\",e[e.Str=3]=\"Str\",e[e.Arr=4]=\"Arr\",e[e.Obj=5]=\"Obj\",e[e.Dot=6]=\"Dot\",e[e.AssignProp=7]=\"AssignProp\",e[e.Ix=8]=\"Ix\",e[e.Add=9]=\"Add\",e[e.Subtract=10]=\"Subtract\",e[e.Multiply=11]=\"Multiply\",e[e.Divide=12]=\"Divide\",e[e.Id=13]=\"Id\",e[e.Lam=14]=\"Lam\",e[e.Arg=15]=\"Arg\",e[e.Apply=16]=\"Apply\",e[e.Call=17]=\"Call\",e[e.AssignVar=18]=\"AssignVar\",e[e.FreeVar=19]=\"FreeVar\",e[e.Var=20]=\"Var\",e[e.InsertNode=21]=\"InsertNode\",e[e.WithBuilder=22]=\"WithBuilder\",e[e.CreateElement=23]=\"CreateElement\",e[e.CreateText=24]=\"CreateText\",e[e.ElementProp=25]=\"ElementProp\",e[e.ElementAttr=26]=\"ElementAttr\",e[e.AddEventListener=27]=\"AddEventListener\",e[e.ToggleClass=28]=\"ToggleClass\",e[e.AssignText=29]=\"AssignText\",e[e.InsertBoundary=30]=\"InsertBoundary\",e[e.ClearBoundary=31]=\"ClearBoundary\",e[e.RevSeq=32]=\"RevSeq\",e[e.ExecCallback=33]=\"ExecCallback\",e[e.UncaughtException=34]=\"UncaughtException\"}(z||(z={}));var W,F=j((function(e){var t;return T(((t={})[z.Null]=V({}),t[z.Boolean]=V({0:I}),t[z.Num]=V({0:D}),t[z.Str]=V({0:N}),t[z.Arr]=V({0:S(e)}),t[z.Obj]=V({0:S(O(N,e))}),t[z.Dot]=V({0:e,1:N}),t[z.AssignProp]=V({0:e,1:N,2:e}),t[z.Ix]=V({exp:e,ix:D}),t[z.Add]=V({0:e,1:e}),t[z.Subtract]=V({0:e,1:e}),t[z.Multiply]=V({0:e,1:e}),t[z.Divide]=V({0:e,1:e}),t[z.Id]=V({0:N}),t[z.Lam]=V({body:e}),t[z.Arg]=V({scopeIx:I,argIx:I}),t[z.Apply]=V({0:e,1:S(e)}),t[z.Call]=V({0:e,1:N,2:S(e)}),t[z.AssignVar]=V({lhs:D,rhs:e}),t[z.FreeVar]=V({varId:D}),t[z.Var]=V({varId:D}),t[z.InsertNode]=V({parent:e,child:e}),t[z.WithBuilder]=V({builder:e,builderContent:e}),t[z.CreateElement]=V({tagName:N}),t[z.CreateText]=V({content:N}),t[z.ElementProp]=V({node:e,propName:N,propValue:e}),t[z.ElementAttr]=V({node:e,attrName:N,attrValue:N}),t[z.AddEventListener]=V({node:e,eventName:N,listener:e}),t[z.ToggleClass]=V({node:e,className:N,enable:I}),t[z.AssignText]=V({node:e,content:N}),t[z.InsertBoundary]=V({parent:e}),t[z.ClearBoundary]=V({boundary:e,detach:I}),t[z.RevSeq]=V({exprs:S(e)}),t[z.ExecCallback]=V({callbackId:D,arg:e}),t[z.UncaughtException]=V({message:N}),t))}));!function(e){e[e.Eval=0]=\"Eval\",e[e.HotReload=1]=\"HotReload\",e[e.Exit=2]=\"Exit\"}(W||(W={}));var H,q=T(((k={})[W.Eval]=V({expr:F}),k[W.HotReload]=V({}),k[W.Exit]=V({}),k));!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.ExecCallback=2]=\"ExecCallback\"}(H||(H={}));var G,X=T(((B={})[H.Start]=V({}),B[H.Return]=V({0:M}),B[H.ExecCallback]=V({arg:M,callbackId:D}),B)),K=new Map;function Q(e,t){void 0===t&&(t={tag:H.Start});var r=Z(e,t);switch(r.tag){case W.Eval:var n=R(Y,null,(function(t){return Q(e,t)}),r.expr),i=P(n);return Q(e,{tag:H.Return,0:i});case W.HotReload:return void window.location.reload();case W.Exit:return}c()}!function(e){function t(e,t){e instanceof Comment?e.parentElement.insertBefore(t,e):e.appendChild(t)}function r(e){return e instanceof Comment?e.parentElement:e}function n(e){return e instanceof Comment&&\"ContentBoundary {{\"==e.textContent}e.insertIntoBuilder=t,e.assignProperty=function(e,t,r){e instanceof Comment?e.parentElement[t]=r:e[t]=r},e.assignAttribute=function(e,t,n){r(e).setAttribute(t,n)},e.addEventListener=function(e,t,n){r(e).addEventListener(t,n)},e.toggleClass=function(e,t,n){var i=r(e);n?i.classList.add(t):i.classList.remove(t)},e.insertBoundary=function(e){var r=document.createComment(\"ContentBoundary {{\"),n=document.createComment(\"}}\");return t(e,r),t(e,n),n},e.clearBoundary=function(e,t){for(var r=e,i=0;r.previousSibling&&(0!=i||!n(r.previousSibling));)(s=r.previousSibling)instanceof Comment&&\"}}\"==s.textContent?i++:n(r.previousSibling)&&i--,r.previousSibling.parentNode.removeChild(r.previousSibling);var s;t&&(r.previousSibling.parentNode.removeChild(r.previousSibling),r.parentNode.removeChild(r))}}(G||(G={}));var Y=[window,null];function Z(e,t){var r=function(e,t){var r=t.byteLength,n=e.exports.hs_malloc(t.length+8);return new DataView(e.exports.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.exports.memory.buffer,n+8,r).set(t),n}(e,X.encode(t)),n=function(e,t){var r=new Uint8Array(e.exports.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),i=new Uint8Array(e.exports.memory.buffer,t+8,n).slice().buffer;return e.exports.hs_free(t),new Uint8Array(i)}(e,e.exports.app(r));return q.decode(n)}var $=[window,null];window.startReactor=function(e){return r(this,void 0,void 0,(function(){var t,r,a;return n(this,(function(n){switch(n.label){case 0:return t=new class{start(e){this.inst=e,e.exports._start()}initialize(e){this.inst=e,e.exports._initialize()}constructor(e,t,r){this.args=[],this.env=[],this.fds=[],this.args=e,this.env=t,this.fds=r;let n=this;this.wasiImport={args_sizes_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);r.setUint32(e,n.args.length,!0);let i=0;for(let e of n.args)i+=e.length+1;return r.setUint32(t,i,!0),0},args_get(e,t){let r=new DataView(n.inst.exports.memory.buffer),i=new Uint8Array(n.inst.exports.memory.buffer);for(let s=0;s<n.args.length;s++){r.setUint32(e,t,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(n.args[s]);i.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return 0},environ_sizes_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);r.setUint32(e,n.env.length,!0);let i=0;for(let e of n.env)i+=e.length+1;return r.setUint32(t,i,!0),0},environ_get(e,r){let i=new DataView(n.inst.exports.memory.buffer),s=new Uint8Array(n.inst.exports.memory.buffer);for(let n=0;n<t.length;n++){i.setUint32(e,r,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(t[n]);s.set(a,r),i.setUint8(r+a.length,0),r+=a.length+1}return 0},clock_res_get(e,t){throw\"unimplemented\"},clock_time_get(e,t,r){let i=new DataView(n.inst.exports.memory.buffer);if(0===e)i.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}i.setBigUint64(r,e,!0)}else i.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,i)=>null!=n.fds[e]?n.fds[e].fd_advise(t,r,i):8,fd_allocate:(e,t,r)=>null!=n.fds[e]?n.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=n.fds[e]){let t=n.fds[e].fd_close();return n.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=n.fds[e]?n.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=n.fds[e]){let{ret:r,fdstat:i}=n.fds[e].fd_fdstat_get();return null!=i&&i.write_bytes(new DataView(n.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=n.fds[e]?n.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=n.fds[e]?n.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=n.fds[e]){let{ret:r,filestat:i}=n.fds[e].fd_filestat_get();return null!=i&&i.write_bytes(new DataView(n.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=n.fds[e]?n.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,i)=>null!=n.fds[e]?n.fds[e].fd_filestat_set_times(t,r,i):8,fd_pread(e,t,r,s,a){let o=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=i.read_bytes_array(o,t,r),{ret:f,nread:c}=n.fds[e].fd_pread(l,u,s);return o.setUint32(a,c,!0),f}return 8},fd_prestat_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:i,prestat:s}=n.fds[e].fd_prestat_get();return null!=s&&s.write_bytes(r,t),i}return 8},fd_prestat_dir_name(e,t,r){if(null!=n.fds[e]){let{ret:r,prestat_dir_name:i}=n.fds[e].fd_prestat_dir_name();return null!=i&&new Uint8Array(n.inst.exports.memory.buffer).set(i,t),r}return 8},fd_pwrite(e,t,r,i,a){let o=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=s.read_bytes_array(o,t,r),{ret:f,nwritten:c}=n.fds[e].fd_pwrite(l,u,i);return o.setUint32(a,c,!0),f}return 8},fd_read(e,t,r,s){let a=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let l=i.read_bytes_array(a,t,r),{ret:u,nread:f}=n.fds[e].fd_read(o,l);return a.setUint32(s,f,!0),u}return 8},fd_readdir(e,t,r,i,s){let a=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let l=0;for(;;){let{ret:u,dirent:f}=n.fds[e].fd_readdir_single(i);if(0!=u)return a.setUint32(s,l,!0),u;if(null==f)break;if(r-l<f.head_length()){l=r;break}let c=new ArrayBuffer(f.head_length());if(f.write_head_bytes(new DataView(c),0),o.set(new Uint8Array(c).slice(0,Math.min(c.byteLength,r-l)),t),t+=f.head_length(),l+=f.head_length(),r-l<f.name_length()){l=r;break}f.write_name_bytes(o,t,r-l),t+=f.name_length(),l+=f.name_length(),i=f.d_next}return a.setUint32(s,l,!0),0}return 8},fd_renumber(e,t){if(null!=n.fds[e]&&null!=n.fds[t]){let r=n.fds[t].fd_close();return 0!=r?r:(n.fds[t]=n.fds[e],n.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,i){let s=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:a,offset:o}=n.fds[e].fd_seek(t,r);return s.setBigInt64(i,o,!0),a}return 8},fd_sync:e=>null!=n.fds[e]?n.fds[e].fd_sync():8,fd_tell(e,t){let r=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:i,offset:s}=n.fds[e].fd_tell();return r.setBigUint64(t,s,!0),i}return 8},fd_write(e,t,r,i){let a=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let l=s.read_bytes_array(a,t,r),{ret:u,nwritten:f}=n.fds[e].fd_write(o,l);return a.setUint32(i,f,!0),u}return 8},path_create_directory(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_create_directory(s)}},path_filestat_get(e,t,r,i,s){let a=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let l=new TextDecoder(\"utf-8\").decode(o.slice(r,r+i)),{ret:u,filestat:f}=n.fds[e].path_filestat_get(t,l);return null!=f&&f.write_bytes(a,s),u}return 8},path_filestat_set_times(e,t,r,i,s,a,o){let l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=new TextDecoder(\"utf-8\").decode(l.slice(r,r+i));return n.fds[e].path_filestat_set_times(t,u,s,a,o)}return 8},path_link(e,t,r,i,s,a,o){let l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]&&null!=n.fds[s]){let u=new TextDecoder(\"utf-8\").decode(l.slice(r,r+i)),f=new TextDecoder(\"utf-8\").decode(l.slice(a,a+o));return n.fds[s].path_link(e,t,u,f)}return 8},path_open(e,t,r,i,s,a,o,l,u){let f=new DataView(n.inst.exports.memory.buffer),c=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let d=new TextDecoder(\"utf-8\").decode(c.slice(r,r+i)),{ret:_,fd_obj:p}=n.fds[e].path_open(t,d,s,a,o,l);if(0!=_)return _;n.fds.push(p);let h=n.fds.length-1;return f.setUint32(u,h,!0),0}return 8},path_readlink(e,t,r,i,s,a){let o=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=new TextDecoder(\"utf-8\").decode(l.slice(t,t+r)),{ret:f,data:c}=n.fds[e].path_readlink(u);if(null!=c){if(c.length>s)return o.setUint32(a,0,!0),8;l.set(c,i),o.setUint32(a,c.length,!0)}return f}return 8},path_remove_directory(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_remove_directory(s)}return 8},path_rename(e,t,r,n,i,s){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,i,s){let a=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[r]){let o=new TextDecoder(\"utf-8\").decode(a.slice(e,e+t)),l=new TextDecoder(\"utf-8\").decode(a.slice(i,i+s));return n.fds[r].path_symlink(o,l)}return 8},path_unlink_file(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_unlink_file(s)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw\"exit with exit code \"+e},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){let r=new Uint8Array(n.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"},sock_accept(e,t){throw\"sockets not supported\"}}}}([],[],[new u(new f([])),new u(new f([])),new u(new f([]))]),[4,WebAssembly.compileStreaming(fetch(e))];case 1:return r=n.sent(),[4,WebAssembly.instantiate(r,{wasi_snapshot_preview1:t.wasiImport})];case 2:return a=n.sent(),t.inst=a,a.exports.hs_init(0,0),Q(a),[2]}}))}))},window.startDevClient=function(e){return r(this,void 0,void 0,(function(){var t,i=this;return n(this,(function(s){return(t=new WebSocket(e)).onopen=function(e){var r=X.encode({tag:H.Start});t.send(r)},t.onmessage=function(e){return r(i,void 0,void 0,(function(){var i;return n(this,(function(s){switch(s.label){case 0:return[4,(a=e.data,new Promise((function(e,t){var r=new FileReader;r.onload=function(){var t=r.result,n=new Uint8Array(t);e(n)},r.onerror=function(e){t(e)},r.readAsArrayBuffer(a)})))];case 1:return i=s.sent(),function(e,t){r(this,void 0,void 0,(function(){var r,i;return n(this,(function(n){switch(e.tag){case W.Eval:return r=R($,null,t,e.expr),i=P(r),[2,t({tag:H.Return,0:i})];case W.HotReload:return window.location.reload(),[2];case W.Exit:return[2]}return c(),[2]}))}))}(q.decode(i),(function(e){return t.send(X.encode(e))})),[2]}var a}))}))},t.onerror=function(e){console.error(\"WebSocket error:\",e)},t.onclose=function(e){console.log(\"WebSocket connection closed:\",e)},[2]}))}))}})();"
