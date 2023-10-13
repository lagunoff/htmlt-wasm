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
indexBundleJs = "(()=>{\"use strict\";var e=function(t,r){return e=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])},e(t,r)};function t(t,r){if(\"function\"!=typeof r&&null!==r)throw new TypeError(\"Class extends value \"+String(r)+\" is not a constructor or null\");function n(){this.constructor=t}e(t,r),t.prototype=null===r?Object.create(r):(n.prototype=r.prototype,new n)}function r(e,t,r,n){return new(r||(r=Promise))((function(i,s){function a(e){try{o(n.next(e))}catch(e){s(e)}}function l(e){try{o(n.throw(e))}catch(e){s(e)}}function o(e){var t;e.done?i(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(a,l)}o((n=n.apply(e,t||[])).next())}))}function n(e,t){var r,n,i,s,a={label:0,sent:function(){if(1&i[0])throw i[1];return i[1]},trys:[],ops:[]};return s={next:l(0),throw:l(1),return:l(2)},\"function\"==typeof Symbol&&(s[Symbol.iterator]=function(){return this}),s;function l(l){return function(o){return function(l){if(r)throw new TypeError(\"Generator is already executing.\");for(;s&&(s=0,l[0]&&(a=0)),a;)try{if(r=1,n&&(i=2&l[0]?n.return:l[0]?n.throw||((i=n.return)&&i.call(n),0):n.next)&&!(i=i.call(n,l[1])).done)return i;switch(n=0,i&&(l=[2&l[0],i.value]),l[0]){case 0:case 1:i=l;break;case 4:return a.label++,{value:l[1],done:!1};case 5:a.label++,n=l[1],l=[0];continue;case 7:l=a.ops.pop(),a.trys.pop();continue;default:if(!((i=(i=a.trys).length>0&&i[i.length-1])||6!==l[0]&&2!==l[0])){a=0;continue}if(3===l[0]&&(!i||l[1]>i[0]&&l[1]<i[3])){a.label=l[1];break}if(6===l[0]&&a.label<i[1]){a.label=i[1],i=l;break}if(i&&a.label<i[2]){a.label=i[2],a.ops.push(l);break}i[2]&&a.ops.pop(),a.trys.pop();continue}l=t.call(e,a)}catch(e){l=[6,e],n=0}finally{r=i=0}if(5&l[0])throw l[1];return{value:l[0]?l[1]:void 0,done:!0}}([l,o])}}}Object.create,Object.create,\"function\"==typeof SuppressedError&&SuppressedError;class i{static read_bytes(e,t){let r=new i;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){let n=[];for(let s=0;s<r;s++)n.push(i.read_bytes(e,t+8*s));return n}}class s{static read_bytes(e,t){let r=new s;return r.buf=e.getUint32(t,!0),r.buf_len=e.getUint32(t+4,!0),r}static read_bytes_array(e,t,r){let n=[];for(let i=0;i<r;i++)n.push(s.read_bytes(e,t+8*i));return n}}class a{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class l{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}class o{fd_advise(e,t,r){return-1}fd_allocate(e,t){return-1}fd_close(){return 0}fd_datasync(){return-1}fd_fdstat_get(){return{ret:-1,fdstat:null}}fd_fdstat_set_flags(e){return-1}fd_fdstat_set_rights(e,t){return-1}fd_filestat_get(){return{ret:-1,filestat:null}}fd_filestat_set_size(e){return-1}fd_filestat_set_times(e,t,r){return-1}fd_pread(e,t,r){return{ret:-1,nread:0}}fd_prestat_get(){return{ret:-1,prestat:null}}fd_prestat_dir_name(e,t){return{ret:-1,prestat_dir_name:null}}fd_pwrite(e,t,r){return{ret:-1,nwritten:0}}fd_read(e,t){return{ret:-1,nread:0}}fd_readdir_single(e){return{ret:-1,dirent:null}}fd_seek(e,t){return{ret:-1,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:-1,offset:0n}}fd_write(e,t){return{ret:-1,nwritten:0}}path_create_directory(e){return-1}path_filestat_get(e,t){return{ret:-1,filestat:null}}path_filestat_set_times(e,t,r,n,i){return-1}path_link(e,t,r,n){return-1}path_open(e,t,r,n,i,s){return{ret:-1,fd_obj:null}}path_readlink(e){return{ret:-1,data:null}}path_remove_directory(e){return-1}path_rename(e,t,r){return-1}path_symlink(e,t){return-1}path_unlink_file(e){return-1}}class u extends o{fd_fdstat_get(){return{ret:0,fdstat:new a(4,0)}}fd_read(e,t){let r=0;for(let n of t){if(!(this.file_pos<this.file.data.byteLength))break;{let t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_write(e,t){let r=0;if(this.file.readonly)return{ret:8,nwritten:r};for(let n of t){let t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){let e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class f{open(e){let t=new u(this);return 1&e&&t.fd_seek(0n,2),t}get size(){return BigInt(this.data.byteLength)}stat(){return new l(4,this.size)}truncate(){return this.readonly?63:(this.data=new Uint8Array([]),0)}constructor(e,t){this.data=new Uint8Array(e),this.readonly=!!t?.readonly}}function c(e){throw new Error(\"absurd: unreachable code\")}var d=function(){function e(){}return e.prototype.encode=function(e){var t=x(this,e),r=new Uint8Array(t);return U(this,r,0,e),r},e.prototype.decode=function(e){var t=E(this,e,0),r=t[0];return t[1],r},e}(),_=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),p=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),h=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),m=function(e){function r(){return null!==e&&e.apply(this,arguments)||this}return t(r,e),r}(d),g=function(e){function r(t){var r=e.call(this)||this;return r._element=t,r}return t(r,e),r}(d),y=function(e){function r(t){var r=e.call(this)||this;return r._description=t,r}return t(r,e),r}(d),w=function(e){function r(t){var r=e.call(this)||this;return r._alternatives=t,r}return t(r,e),r}(d),v=function(e){function r(t){var r=e.call(this)||this;return r._self=t,r}return t(r,e),r}(d),b=function(e){function r(t){var r=e.call(this)||this;return r._tuple=t,r}return t(r,e),r}(d);function x(e,t){if(e instanceof _)return 1;if(e instanceof p)return 8;if(e instanceof m){var r=t;return(n=8)+(new TextEncoder).encode(r).length}if(e instanceof h)return(n=8)+t.length;if(e instanceof g){var n=8;return t.reduce((function(t,r){return t+x(e._element,r)}),n)}if(e instanceof y){var i=t;return Object.keys(e._description).reduce((function(t,r){return t+x(e._description[r],i[r])}),0)}if(e instanceof w){var s=t;return A(Object.keys(e._alternatives).length)+x(e._alternatives[s.tag],s)}if(e instanceof v)return x(e._self,t);if(e instanceof b){var a=t;return e._tuple.reduce((function(e,t,r){return e+x(t,a[r])}),0)}return c()}function E(e,t,r){if(e instanceof _)return[t[r],r+1];if(e instanceof p)return[u=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),r+8];if(e instanceof m){var n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),i=t.subarray(r+8,r+8+n);return[new TextDecoder(\"utf8\").decode(i),r+8+n]}if(e instanceof h)return n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),[t.subarray(r+8,r+8+n),r+8+n];if(e instanceof g){n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);for(var s=[],a=r+8,l=0;l<n;l++){var o=E(e._element,t,a),u=o[0],f=o[1];s.push(u),a=f}return[s,a]}if(e instanceof y){var d=r,x=Object.fromEntries(Object.entries(e._description).map((function(e){var r=e[0],n=E(e[1],t,d),i=n[0],s=n[1];return d=s,[r,i]})));return[x,d]}if(e instanceof w){var U=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(A(Object.keys(e._alternatives).length),t,r),I=U[0],k=U[1],B=E(e._alternatives[I],t,k),D=B[0],S=B[1];return D.tag=I,[D,S]}if(e instanceof v)return E(e._self,t,r);if(e instanceof b){var N=r,V=e._tuple.map((function(e){var r=E(e,t,N),n=r[0],i=r[1];return N=i,n}));return[V,N]}return c()}function U(e,t,r,n){if(e instanceof _)return t[r]=n,r+1;if(e instanceof p){var i=n;return t[r+7]=255&i,t[r+6]=i>>8&255,t[r+5]=i>>16&255,t[r+4]=i>>24&255,r+8}if(e instanceof m){var s=n,a=(new TextEncoder).encode(s),l=a.length;return t[r+7]=255&l,t[r+6]=l>>8&255,t[r+5]=l>>16&255,t[r+4]=l>>24&255,t.set(a,r+8),r+8+l}if(e instanceof h){var o=n;return l=o.length,t[r+7]=255&l,t[r+6]=l>>8&255,t[r+5]=l>>16&255,t[r+4]=l>>24&255,t.set(o,r+8),r+8+l}if(e instanceof g){var u=n;l=u.length,t[r+7]=255&l,t[r+6]=l>>8&255,t[r+5]=l>>16&255,t[r+4]=l>>24&255;for(var f=r+8,d=0;d<l;d++)f=U(e._element,t,f,u[d]);return f}if(e instanceof y){var x=n;for(var E in f=r,e._description)Object.prototype.hasOwnProperty.call(e._description,E)&&(f=U(e._description[E],t,f,x[E]));return f}if(e instanceof w){var I=n.tag,k=A(Object.keys(e._alternatives).length);return t[r]=I,U(e._alternatives[I],t,r+k,n)}if(e instanceof v)return U(e._self,t,r,n);if(e instanceof b){var B=n,D=r;return e._tuple.forEach((function(e,r){D=U(e,t,D,B[r])})),D}return c()}function A(e){return Math.ceil(Math.log2(e)/8)}var I,k,B,D=new _,S=new p,N=new m;function V(e){return new g(e)}function C(e){return new y(e)}function T(e){return new w(e)}function O(){for(var e=[],t=0;t<arguments.length;t++)e[t]=arguments[t];return new b(e)}function P(e){var t=new v(void 0),r=e(t);return t._self=r,r}function j(e,t,r){switch(r.tag){case L.Null:return null;case L.Boolean:return 0!=r[0];case L.Num:case L.Str:return r[0];case L.Arr:return r[0].map(j.bind(void 0,e,t));case L.Obj:return Object.fromEntries(r[0].map((function(r){var n=r[0],i=r[1];return[n,j(e,t,i)]})));case L.Dot:return(o=j(e,t,r[0]))[r[1]];case L.AssignProp:var n=j(e,t,r[2]);return j(e,t,r[0])[r[1]]=n,n;case L.Ix:return(n=j(e,t,r.exp))[r.ix];case L.Add:return(o=j(e,t,r[0]))+j(e,t,r[1]);case L.Subtract:return(o=j(e,t,r[0]))-j(e,t,r[1]);case L.Multiply:return(o=j(e,t,r[0]))*j(e,t,r[1]);case L.Divide:return(o=j(e,t,r[0]))/j(e,t,r[1]);case L.Id:for(var i=r[0],s=e;s;s=s[1]){var a=s[0];if(i in a)return a[i]}throw new Error(\"Variable not in scope: \"+r[0]);case L.Lam:var l=r.args;return function(){for(var n=[],i=0;i<arguments.length;i++)n[i]=arguments[i];return j([l.reduce((function(e,t,r){return e[t]=n[r],e}),{}),e],t,r.body)};case L.Apply:return(o=j(e,t,r[0])).apply(void 0,r[1].map(j.bind(void 0,e,t)));case L.Call:var o;return(o=j(e,t,r[0]))[r[1]].apply(o,r[2].map(j.bind(void 0,e,t)));case L.AssignVar:return n=j(e,t,r.rhs),q.set(r.lhs,n),n;case L.FreeVar:return q.delete(r.varId);case L.Var:return q.get(r.varId);case L.ElInitBuilder:var u=j(e,t,r.element),f=new G(null,u);return q.set(r.varId,f),f;case L.ElDestroyBuilder:return(d=q.get(r.varId))instanceof X&&(K(d),d._begin.parentElement.removeChild(d._begin),d._end.parentElement.removeChild(d._end)),q.delete(r.varId),null;case L.ElPush:return f=function(e,t){var r=document.createElement(t);return Q(e,r),new G(e,r)}(d=q.get(r.varId),r.tagName),q.set(r.varId,f),f;case L.ElNoPush:return Q(d=q.get(r.varId),document.createElement(r.tagName)),null;case L.ElProp:var d=q.get(r.varId),_=j(e,t,r.val);return function(e,t,r){e instanceof G?e._element[t]=r:e instanceof X?e._end.parentElement[t]=r:c()}(d,r.prop,_),null;case L.ElAttr:return function(e,t,r){e instanceof G?e._element.setAttribute(t,r):e instanceof X?e._end.parentElement.setAttribute(t,r):c()}(d=q.get(r.varId),r.attr,r.val),null;case L.ElEvent:d=q.get(r.varId);var p=j(e,t,r.callback);return function(e,t,r){e instanceof G?e._element.addEventListener(t,r):e instanceof X?e._end.parentElement.addEventListener(t,r):c()}(d,r.name,p),null;case L.ElText:return Q(d=q.get(r.varId),h=document.createTextNode(r.content)),h;case L.ElAssignTextContent:var h;return(h=q.get(r.varId)).nodeValue=r.content,null;case L.ElPop:return(d=q.get(r.varId))instanceof G||d instanceof X?(q.set(r.varId,d._parent),null):c();case L.ElInsertBoundary:return f=function(e){var t=document.createComment(\"ContentBoundary {{\"),r=document.createComment(\"}}\");return Q(e,t),Q(e,r),new X(e,t,r)}(d=q.get(r.varId)),q.set(r.varId,f),f;case L.ElClearBoundary:return(d=q.get(r.varId))instanceof X?(K(d),null):null;case L.ElToggleClass:return function(e,t,r){e instanceof G?r?e._element.classList.add(t):e._element.classList.remove(t):e instanceof X?r?e._end.parentElement.classList.add(t):e._end.parentElement.classList.remove(t):c()}(d=q.get(r.varId),r.className,0!=r.enable),null;case L.RevSeq:return r.exprs.reduceRight((function(r,n){return j(e,t,n)}),null);case L.ExecCallback:var m=j(e,t,r.arg);return t({tag:F.ExecCallback,arg:J(m),callbackId:r.callbackId});case L.UncaughtException:throw new Error(r.message)}c()}function J(e){if(\"boolean\"==typeof e)return{tag:B.JBool,0:e?1:0};if(\"number\"==typeof e)return{tag:B.JNum,0:e};if(\"string\"==typeof e)return{tag:B.JStr,0:e};if(Array.isArray(e))return{tag:B.JArr,0:e.map(J)};if(null==e)return{tag:B.JNull};var t=Object.entries(e).map((function(e){return[e[0],J(e[1])]}));return{tag:B.JObj,0:t}}new h,function(e){e[e.JNull=0]=\"JNull\",e[e.JBool=1]=\"JBool\",e[e.JNum=2]=\"JNum\",e[e.JStr=3]=\"JStr\",e[e.JArr=4]=\"JArr\",e[e.JObj=5]=\"JObj\"}(B||(B={}));var L,R=P((function(e){var t;return T(((t={})[B.JNull]=C({}),t[B.JBool]=C({0:D}),t[B.JNum]=C({0:S}),t[B.JStr]=C({0:N}),t[B.JArr]=C({0:V(e)}),t[B.JObj]=C({0:V(O(N,e))}),t))}));!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.Num=2]=\"Num\",e[e.Str=3]=\"Str\",e[e.Arr=4]=\"Arr\",e[e.Obj=5]=\"Obj\",e[e.Dot=6]=\"Dot\",e[e.AssignProp=7]=\"AssignProp\",e[e.Ix=8]=\"Ix\",e[e.Add=9]=\"Add\",e[e.Subtract=10]=\"Subtract\",e[e.Multiply=11]=\"Multiply\",e[e.Divide=12]=\"Divide\",e[e.Id=13]=\"Id\",e[e.Lam=14]=\"Lam\",e[e.Apply=15]=\"Apply\",e[e.Call=16]=\"Call\",e[e.AssignVar=17]=\"AssignVar\",e[e.FreeVar=18]=\"FreeVar\",e[e.Var=19]=\"Var\",e[e.ElInitBuilder=20]=\"ElInitBuilder\",e[e.ElDestroyBuilder=21]=\"ElDestroyBuilder\",e[e.ElPush=22]=\"ElPush\",e[e.ElNoPush=23]=\"ElNoPush\",e[e.ElProp=24]=\"ElProp\",e[e.ElAttr=25]=\"ElAttr\",e[e.ElEvent=26]=\"ElEvent\",e[e.ElText=27]=\"ElText\",e[e.ElAssignTextContent=28]=\"ElAssignTextContent\",e[e.ElPop=29]=\"ElPop\",e[e.ElInsertBoundary=30]=\"ElInsertBoundary\",e[e.ElClearBoundary=31]=\"ElClearBoundary\",e[e.ElToggleClass=32]=\"ElToggleClass\",e[e.RevSeq=33]=\"RevSeq\",e[e.ExecCallback=34]=\"ExecCallback\",e[e.UncaughtException=35]=\"UncaughtException\"}(L||(L={}));var z,M=P((function(e){var t;return T(((t={})[L.Null]=C({}),t[L.Boolean]=C({0:D}),t[L.Num]=C({0:S}),t[L.Str]=C({0:N}),t[L.Arr]=C({0:V(e)}),t[L.Obj]=C({0:V(O(N,e))}),t[L.Dot]=C({0:e,1:N}),t[L.AssignProp]=C({0:e,1:N,2:e}),t[L.Ix]=C({exp:e,ix:S}),t[L.Add]=C({0:e,1:e}),t[L.Subtract]=C({0:e,1:e}),t[L.Multiply]=C({0:e,1:e}),t[L.Divide]=C({0:e,1:e}),t[L.Id]=C({0:N}),t[L.Lam]=C({args:V(N),body:e}),t[L.Apply]=C({0:e,1:V(e)}),t[L.Call]=C({0:e,1:N,2:V(e)}),t[L.AssignVar]=C({lhs:S,rhs:e}),t[L.FreeVar]=C({varId:S}),t[L.Var]=C({varId:S}),t[L.ElInitBuilder]=C({varId:S,element:e}),t[L.ElDestroyBuilder]=C({varId:S}),t[L.ElPush]=C({varId:S,tagName:N}),t[L.ElNoPush]=C({varId:S,tagName:N}),t[L.ElProp]=C({varId:S,prop:N,val:e}),t[L.ElAttr]=C({varId:S,attr:N,val:N}),t[L.ElEvent]=C({varId:S,name:N,callback:e}),t[L.ElText]=C({varId:S,content:N}),t[L.ElAssignTextContent]=C({varId:S,content:N}),t[L.ElPop]=C({varId:S}),t[L.ElInsertBoundary]=C({varId:S}),t[L.ElClearBoundary]=C({varId:S}),t[L.ElToggleClass]=C({varId:S,className:N,enable:D}),t[L.RevSeq]=C({exprs:V(e)}),t[L.ExecCallback]=C({callbackId:S,arg:e}),t[L.UncaughtException]=C({message:N}),t))}));!function(e){e[e.Eval=0]=\"Eval\",e[e.HotReload=1]=\"HotReload\",e[e.Exit=2]=\"Exit\"}(z||(z={}));var F,H=T(((I={})[z.Eval]=C({expr:M}),I[z.HotReload]=C({}),I[z.Exit]=C({}),I));!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.ExecCallback=2]=\"ExecCallback\"}(F||(F={}));var W=T(((k={})[F.Start]=C({}),k[F.Return]=C({0:R}),k[F.ExecCallback]=C({arg:R,callbackId:S}),k)),q=new Map,G=function(e,t){this._parent=e,this._element=t},X=function(e,t,r){this._parent=e,this._begin=t,this._end=r};function K(e){for(var t=[e._begin,e._end],r=t[0],n=t[1];n.previousSibling&&n.previousSibling.parentNode&&n.previousSibling!==r;)n.previousSibling.parentNode.removeChild(n.previousSibling)}function Q(e,t){if(e instanceof G)e._element.appendChild(t);else{if(!(e instanceof X))return c();e._end.parentElement.insertBefore(t,e._end)}}function Y(e,t){void 0===t&&(t={tag:F.Start});var r=$(e,t);switch(r.tag){case z.Eval:var n=j(Z,(function(t){return Y(e,t)}),r.expr),i=J(n);return Y(e,{tag:F.Return,0:i});case z.HotReload:return void window.location.reload();case z.Exit:return}c()}var Z=[window,null];function $(e,t){var r=W.encode(t);console.log(\"sending \".concat(r.length,\" bytes\"));var n=function(e,t){var r=t.byteLength,n=e.exports.hs_malloc(t.length+8);return new DataView(e.exports.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.exports.memory.buffer,n+8,r).set(t),n}(e,r),i=function(e,t){var r=new Uint8Array(e.exports.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),i=new Uint8Array(e.exports.memory.buffer,t+8,n).slice().buffer;return e.exports.hs_free(t),new Uint8Array(i)}(e,e.exports.app(n));return console.log(\"receiving \".concat(i.length,\" bytes\")),H.decode(i)}var ee=[window,null];window.startReactor=function(e){return r(this,void 0,void 0,(function(){var t,r,a;return n(this,(function(n){switch(n.label){case 0:return t=new class{start(e){this.inst=e,e.exports._start()}initialize(e){this.inst=e,e.exports._initialize()}constructor(e,t,r){this.args=[],this.env=[],this.fds=[],this.args=e,this.env=t,this.fds=r;let n=this;this.wasiImport={args_sizes_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);r.setUint32(e,n.args.length,!0);let i=0;for(let e of n.args)i+=e.length+1;return r.setUint32(t,i,!0),0},args_get(e,t){let r=new DataView(n.inst.exports.memory.buffer),i=new Uint8Array(n.inst.exports.memory.buffer);for(let s=0;s<n.args.length;s++){r.setUint32(e,t,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(n.args[s]);i.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return 0},environ_sizes_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);r.setUint32(e,n.env.length,!0);let i=0;for(let e of n.env)i+=e.length+1;return r.setUint32(t,i,!0),0},environ_get(e,r){let i=new DataView(n.inst.exports.memory.buffer),s=new Uint8Array(n.inst.exports.memory.buffer);for(let n=0;n<t.length;n++){i.setUint32(e,r,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(t[n]);s.set(a,r),i.setUint8(r+a.length,0),r+=a.length+1}return 0},clock_res_get(e,t){throw\"unimplemented\"},clock_time_get(e,t,r){let i=new DataView(n.inst.exports.memory.buffer);if(0===e)i.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}i.setBigUint64(r,e,!0)}else i.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,i)=>null!=n.fds[e]?n.fds[e].fd_advise(t,r,i):8,fd_allocate:(e,t,r)=>null!=n.fds[e]?n.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=n.fds[e]){let t=n.fds[e].fd_close();return n.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=n.fds[e]?n.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=n.fds[e]){let{ret:r,fdstat:i}=n.fds[e].fd_fdstat_get();return null!=i&&i.write_bytes(new DataView(n.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=n.fds[e]?n.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=n.fds[e]?n.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=n.fds[e]){let{ret:r,filestat:i}=n.fds[e].fd_filestat_get();return null!=i&&i.write_bytes(new DataView(n.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=n.fds[e]?n.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,i)=>null!=n.fds[e]?n.fds[e].fd_filestat_set_times(t,r,i):8,fd_pread(e,t,r,s,a){let l=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=i.read_bytes_array(l,t,r),{ret:f,nread:c}=n.fds[e].fd_pread(o,u,s);return l.setUint32(a,c,!0),f}return 8},fd_prestat_get(e,t){let r=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:i,prestat:s}=n.fds[e].fd_prestat_get();return null!=s&&s.write_bytes(r,t),i}return 8},fd_prestat_dir_name(e,t,r){if(null!=n.fds[e]){let{ret:r,prestat_dir_name:i}=n.fds[e].fd_prestat_dir_name();return null!=i&&new Uint8Array(n.inst.exports.memory.buffer).set(i,t),r}return 8},fd_pwrite(e,t,r,i,a){let l=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=s.read_bytes_array(l,t,r),{ret:f,nwritten:c}=n.fds[e].fd_pwrite(o,u,i);return l.setUint32(a,c,!0),f}return 8},fd_read(e,t,r,s){let a=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let o=i.read_bytes_array(a,t,r),{ret:u,nread:f}=n.fds[e].fd_read(l,o);return a.setUint32(s,f,!0),u}return 8},fd_readdir(e,t,r,i,s){let a=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let o=0;for(;;){let{ret:u,dirent:f}=n.fds[e].fd_readdir_single(i);if(0!=u)return a.setUint32(s,o,!0),u;if(null==f)break;if(r-o<f.head_length()){o=r;break}let c=new ArrayBuffer(f.head_length());if(f.write_head_bytes(new DataView(c),0),l.set(new Uint8Array(c).slice(0,Math.min(c.byteLength,r-o)),t),t+=f.head_length(),o+=f.head_length(),r-o<f.name_length()){o=r;break}f.write_name_bytes(l,t,r-o),t+=f.name_length(),o+=f.name_length(),i=f.d_next}return a.setUint32(s,o,!0),0}return 8},fd_renumber(e,t){if(null!=n.fds[e]&&null!=n.fds[t]){let r=n.fds[t].fd_close();return 0!=r?r:(n.fds[t]=n.fds[e],n.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,i){let s=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:a,offset:l}=n.fds[e].fd_seek(t,r);return s.setBigInt64(i,l,!0),a}return 8},fd_sync:e=>null!=n.fds[e]?n.fds[e].fd_sync():8,fd_tell(e,t){let r=new DataView(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let{ret:i,offset:s}=n.fds[e].fd_tell();return r.setBigUint64(t,s,!0),i}return 8},fd_write(e,t,r,i){let a=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let o=s.read_bytes_array(a,t,r),{ret:u,nwritten:f}=n.fds[e].fd_write(l,o);return a.setUint32(i,f,!0),u}return 8},path_create_directory(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_create_directory(s)}},path_filestat_get(e,t,r,i,s){let a=new DataView(n.inst.exports.memory.buffer),l=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let o=new TextDecoder(\"utf-8\").decode(l.slice(r,r+i)),{ret:u,filestat:f}=n.fds[e].path_filestat_get(t,o);return null!=f&&f.write_bytes(a,s),u}return 8},path_filestat_set_times(e,t,r,i,s,a,l){let o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=new TextDecoder(\"utf-8\").decode(o.slice(r,r+i));return n.fds[e].path_filestat_set_times(t,u,s,a,l)}return 8},path_link(e,t,r,i,s,a,l){let o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]&&null!=n.fds[s]){let u=new TextDecoder(\"utf-8\").decode(o.slice(r,r+i)),f=new TextDecoder(\"utf-8\").decode(o.slice(a,a+l));return n.fds[s].path_link(e,t,u,f)}return 8},path_open(e,t,r,i,s,a,l,o,u){let f=new DataView(n.inst.exports.memory.buffer),c=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let d=new TextDecoder(\"utf-8\").decode(c.slice(r,r+i)),{ret:_,fd_obj:p}=n.fds[e].path_open(t,d,s,a,l,o);if(0!=_)return _;n.fds.push(p);let h=n.fds.length-1;return f.setUint32(u,h,!0),0}return 8},path_readlink(e,t,r,i,s,a){let l=new DataView(n.inst.exports.memory.buffer),o=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let u=new TextDecoder(\"utf-8\").decode(o.slice(t,t+r)),{ret:f,data:c}=n.fds[e].path_readlink(u);if(null!=c){if(c.length>s)return l.setUint32(a,0,!0),8;o.set(c,i),l.setUint32(a,c.length,!0)}return f}return 8},path_remove_directory(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_remove_directory(s)}return 8},path_rename(e,t,r,n,i,s){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,i,s){let a=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[r]){let l=new TextDecoder(\"utf-8\").decode(a.slice(e,e+t)),o=new TextDecoder(\"utf-8\").decode(a.slice(i,i+s));return n.fds[r].path_symlink(l,o)}return 8},path_unlink_file(e,t,r){let i=new Uint8Array(n.inst.exports.memory.buffer);if(null!=n.fds[e]){let s=new TextDecoder(\"utf-8\").decode(i.slice(t,t+r));return n.fds[e].path_unlink_file(s)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw\"exit with exit code \"+e},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){let r=new Uint8Array(n.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"},sock_accept(e,t){throw\"sockets not supported\"}}}}([],[],[new u(new f([])),new u(new f([])),new u(new f([]))]),[4,WebAssembly.compileStreaming(fetch(e))];case 1:return r=n.sent(),[4,WebAssembly.instantiate(r,{wasi_snapshot_preview1:t.wasiImport})];case 2:return a=n.sent(),t.inst=a,a.exports.hs_init(0,0),Y(a),[2]}}))}))},window.startDevClient=function(e){return r(this,void 0,void 0,(function(){var t,i=this;return n(this,(function(s){return(t=new WebSocket(e)).onopen=function(e){var r=W.encode({tag:F.Start});t.send(r)},t.onmessage=function(e){return r(i,void 0,void 0,(function(){var i;return n(this,(function(s){switch(s.label){case 0:return[4,(a=e.data,new Promise((function(e,t){var r=new FileReader;r.onload=function(){var t=r.result,n=new Uint8Array(t);e(n)},r.onerror=function(e){t(e)},r.readAsArrayBuffer(a)})))];case 1:return i=s.sent(),function(e,t){r(this,void 0,void 0,(function(){var r,i;return n(this,(function(n){switch(e.tag){case z.Eval:return r=j(ee,t,e.expr),i=J(r),[2,t({tag:F.Return,0:i})];case z.HotReload:return window.location.reload(),[2];case z.Exit:return[2]}return c(),[2]}))}))}(H.decode(i),(function(e){return t.send(W.encode(e))})),[2]}var a}))}))},t.onerror=function(e){console.error(\"WebSocket error:\",e)},t.onclose=function(e){console.log(\"WebSocket connection closed:\",e)},[2]}))}))}})();"
