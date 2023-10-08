module HtmlT.Wasm.DevServer where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function
import Data.IORef
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Foreign.Store
import Network.HTTP.Types as H
import Network.Wai as WAI
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets

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
      \  <script>\n\
      \    " <> BSL.fromStrict runtimeJs <> "\n\
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

runtimeJs :: ByteString
runtimeJs = "(()=>{\"use strict\";class e{static read_bytes(t,r){let n=new e;return n.buf=t.getUint32(r,!0),n.buf_len=t.getUint32(r+4,!0),n}static read_bytes_array(t,r,n){let s=[];for(let i=0;i<n;i++)s.push(e.read_bytes(t,r+8*i));return s}}class t{static read_bytes(e,r){let n=new t;return n.buf=e.getUint32(r,!0),n.buf_len=e.getUint32(r+4,!0),n}static read_bytes_array(e,r,n){let s=[];for(let i=0;i<n;i++)s.push(t.read_bytes(e,r+8*i));return s}}class r{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class n{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}class s{fd_advise(e,t,r){return-1}fd_allocate(e,t){return-1}fd_close(){return 0}fd_datasync(){return-1}fd_fdstat_get(){return{ret:-1,fdstat:null}}fd_fdstat_set_flags(e){return-1}fd_fdstat_set_rights(e,t){return-1}fd_filestat_get(){return{ret:-1,filestat:null}}fd_filestat_set_size(e){return-1}fd_filestat_set_times(e,t,r){return-1}fd_pread(e,t,r){return{ret:-1,nread:0}}fd_prestat_get(){return{ret:-1,prestat:null}}fd_prestat_dir_name(e,t){return{ret:-1,prestat_dir_name:null}}fd_pwrite(e,t,r){return{ret:-1,nwritten:0}}fd_read(e,t){return{ret:-1,nread:0}}fd_readdir_single(e){return{ret:-1,dirent:null}}fd_seek(e,t){return{ret:-1,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:-1,offset:0n}}fd_write(e,t){return{ret:-1,nwritten:0}}path_create_directory(e){return-1}path_filestat_get(e,t){return{ret:-1,filestat:null}}path_filestat_set_times(e,t,r,n,s){return-1}path_link(e,t,r,n){return-1}path_open(e,t,r,n,s,i){return{ret:-1,fd_obj:null}}path_readlink(e){return{ret:-1,data:null}}path_remove_directory(e){return-1}path_rename(e,t,r){return-1}path_symlink(e,t){return-1}path_unlink_file(e){return-1}}class i extends s{fd_fdstat_get(){return{ret:0,fdstat:new r(4,0)}}fd_read(e,t){let r=0;for(let n of t){if(!(this.file_pos<this.file.data.byteLength))break;{let t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_write(e,t){let r=0;for(let n of t){let t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){let e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class a{get size(){return BigInt(this.data.byteLength)}stat(){return new n(4,this.size)}truncate(){this.data=new Uint8Array([])}constructor(e){this.data=new Uint8Array(e)}}function l(e){throw new Error(\"absurd: unreachable code\")}class o{encode(e){const t=w(this,e),r=new Uint8Array(t);return b(this,r,0,e),r}decode(e){const[t,r]=y(this,e,0);return t}}class f extends o{}class u extends o{}class d extends o{}class c extends o{}class _ extends o{constructor(e){super(),this._element=e}}class p extends o{constructor(e){super(),this._description=e}}class h extends o{constructor(e){super(),this._alternatives=e}}class m extends o{constructor(e){super(),this._self=e}}class g extends o{constructor(e){super(),this._tuple=e}}function w(e,t){if(e instanceof f)return 1;if(e instanceof u)return 8;if(e instanceof c){const e=t;return 8+(new TextEncoder).encode(e).length}if(e instanceof d)return 8+t.length;if(e instanceof _){const r=8;return t.reduce(((t,r)=>t+w(e._element,r)),r)}if(e instanceof p){const r=t;return Object.keys(e._description).reduce(((t,n)=>t+w(e._description[n],r[n])),0)}if(e instanceof h){const r=t;return v(Object.keys(e._alternatives).length)+w(e._alternatives[r.tag],r)}if(e instanceof m)return w(e._self,t);if(e instanceof g){const r=t;return e._tuple.reduce(((e,t,n)=>e+w(t,r[n])),0)}return l()}function y(e,t,r){if(e instanceof f)return[t[r],r+1];if(e instanceof u)return[t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),r+8];if(e instanceof c){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),n=t.subarray(r+8,r+8+e);return[new TextDecoder(\"utf8\").decode(n),r+8+e]}if(e instanceof d){const e=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);return[t.subarray(r+8,r+8+e),r+8+e]}if(e instanceof _){const n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),s=[];let i=r+8;for(let r=0;r<n;r++){const[r,n]=y(e._element,t,i);s.push(r),i=n}return[s,i]}if(e instanceof p){let n=r;return[Object.fromEntries(Object.entries(e._description).map((([e,r])=>{const[s,i]=y(r,t,n);return n=i,[e,s]}))),n]}if(e instanceof h){const n=v(Object.keys(e._alternatives).length),[s,i]=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(n,t,r),[a,l]=y(e._alternatives[s],t,i);return a.tag=s,[a,l]}if(e instanceof m)return y(e._self,t,r);if(e instanceof g){let n=r;return[e._tuple.map((e=>{const[r,s]=y(e,t,n);return n=s,r})),n]}return l()}function b(e,t,r,n){if(e instanceof f)return t[r]=n,r+1;if(e instanceof u){const e=n;return t[r+7]=255&e,t[r+6]=e>>8&255,t[r+5]=e>>16&255,t[r+4]=e>>24&255,r+8}if(e instanceof c){const e=n,s=(new TextEncoder).encode(e),i=s.length;return t[r+7]=255&i,t[r+6]=i>>8&255,t[r+5]=i>>16&255,t[r+4]=i>>24&255,t.set(s,r+8),r+8+i}if(e instanceof d){const e=n,s=e.length;return t[r+7]=255&s,t[r+6]=s>>8&255,t[r+5]=s>>16&255,t[r+4]=s>>24&255,t.set(e,r+8),r+8+s}if(e instanceof _){const s=n,i=s.length;t[r+7]=255&i,t[r+6]=i>>8&255,t[r+5]=i>>16&255,t[r+4]=i>>24&255;let a=r+8;for(let r=0;r<i;r++)a=b(e._element,t,a,s[r]);return a}if(e instanceof p){const s=n;let i=r;for(const r in e._description)Object.prototype.hasOwnProperty.call(e._description,r)&&(i=b(e._description[r],t,i,s[r]));return i}if(e instanceof h){const s=n.tag,i=v(Object.keys(e._alternatives).length);return t[r]=s,b(e._alternatives[s],t,r+i,n)}if(e instanceof m)return b(e._self,t,r,n);if(e instanceof g){const s=n;let i=r;return e._tuple.forEach(((e,r)=>{i=b(e,t,i,s[r])})),i}return l()}function v(e){return Math.ceil(Math.log2(e)/8)}const x=new f,E=new u,U=new c;function A(e){return new _(e)}function I(e){return new p(e)}function k(e){return new h(e)}function B(...e){return new g(e)}function D(e){const t=new m(void 0),r=e(t);return t._self=r,r}function N(e,t,r){switch(r.tag){case T.Null:return null;case T.Boolean:return 0!=r[0];case T.Num:case T.Str:return r[0];case T.Arr:return r[0].map(N.bind(void 0,e,t));case T.Obj:return Object.fromEntries(r[0].map((([r,n])=>[r,N(e,t,n)])));case T.Dot:return N(e,t,r[0])[r[1]];case T.AssignProp:{const n=N(e,t,r[2]);return N(e,t,r[0])[r[1]]=n,n}case T.Ix:return N(e,t,r.exp)[r.ix];case T.Add:return N(e,t,r[0])+N(e,t,r[1]);case T.Subtract:return N(e,t,r[0])-N(e,t,r[1]);case T.Multiply:return N(e,t,r[0])*N(e,t,r[1]);case T.Divide:return N(e,t,r[0])/N(e,t,r[1]);case T.Id:{const t=r[0];for(let r=e;r;r=r[1]){const e=r[0];if(t in e)return e[t]}throw new Error(\"Variable not in scope: \"+r[0])}case T.Lam:{const n=r.args;return(...s)=>N([n.reduce(((e,t,r)=>(e[t]=s[r],e)),{}),e],t,r.body)}case T.Apply:return N(e,t,r[0]).apply(void 0,r[1].map(N.bind(void 0,e,t)));case T.Call:{const n=N(e,t,r[0]);return n[r[1]].apply(n,r[2].map(N.bind(void 0,e,t)))}case T.AssignVar:{const n=N(e,t,r.rhs);return R.set(r.lhs,n),n}case T.FreeVar:return R.delete(r.varId);case T.Var:return R.get(r.varId);case T.ElInitBuilder:{const n=N(e,t,r.element),s=new z(null,n);return R.set(r.varId,s),s}case T.ElDestroyBuilder:{const e=R.get(r.varId);return e instanceof M&&(F(e),e._begin.parentElement.removeChild(e._begin),e._end.parentElement.removeChild(e._end)),R.delete(r.varId),null}case T.ElPush:{const e=function(e,t){const r=document.createElement(t);return H(e,r),new z(e,r)}(R.get(r.varId),r.tagName);return R.set(r.varId,e),e}case T.ElNoPush:return H(R.get(r.varId),document.createElement(r.tagName)),null;case T.ElProp:{const n=R.get(r.varId),s=N(e,t,r.val);return function(e,t,r){e instanceof z?e._element[t]=r:e instanceof M?e._end.parentElement[t]=r:l()}(n,r.prop,s),null}case T.ElAttr:return n=R.get(r.varId),s=r.attr,i=r.val,n instanceof z?n._element.setAttribute(s,i):n instanceof M?n._end.parentElement.setAttribute(s,i):l(),null;case T.ElEvent:{const n=R.get(r.varId),s=N(e,t,r.callback);return function(e,t,r){e instanceof z?e._element.addEventListener(t,r):e instanceof M?e._end.parentElement.addEventListener(t,r):l()}(n,r.name,s),null}case T.ElText:{const e=R.get(r.varId),t=document.createTextNode(r.content);return H(e,t),t}case T.ElAssignTextContent:return R.get(r.varId).nodeValue=r.content,null;case T.ElPop:{const e=R.get(r.varId);return e instanceof z||e instanceof M?(R.set(r.varId,e._parent),null):l()}case T.ElInsertBoundary:{const e=function(e){const t=document.createComment(\"ContentBoundary {{\"),r=document.createComment(\"}}\");return H(e,t),H(e,r),new M(e,t,r)}(R.get(r.varId));return R.set(r.varId,e),e}case T.ElClearBoundary:{const e=R.get(r.varId);return e instanceof M?(F(e),null):null}case T.ElToggleClass:return function(e,t,r){e instanceof z?r?e._element.classList.add(t):e._element.classList.remove(t):e instanceof M?r?e._end.parentElement.classList.add(t):e._end.parentElement.classList.remove(t):l()}(R.get(r.varId),r.className,0!=r.enable),null;case T.RevSeq:return r.exprs.reduceRight(((r,n)=>N(e,t,n)),null);case T.ExecCallback:{const n=N(e,t,r.arg);return t({tag:L.ExecCallback,arg:V(n),callbackId:r.callbackId})}case T.UncaughtException:throw new Error(r.message)}var n,s,i;l()}function V(e){if(\"boolean\"==typeof e)return{tag:C.JBool,0:e?1:0};if(\"number\"==typeof e)return{tag:C.JNum,0:e};if(\"string\"==typeof e)return{tag:C.JStr,0:e};if(Array.isArray(e))return{tag:C.JArr,0:e.map(V)};if(null==e)return{tag:C.JNull};const t=Object.entries(e).map((([e,t])=>[e,V(t)]));return{tag:C.JObj,0:t}}var C;new d,function(e){e[e.JNull=0]=\"JNull\",e[e.JBool=1]=\"JBool\",e[e.JNum=2]=\"JNum\",e[e.JStr=3]=\"JStr\",e[e.JArr=4]=\"JArr\",e[e.JObj=5]=\"JObj\"}(C||(C={}));const S=D((e=>k({[C.JNull]:I({}),[C.JBool]:I({0:x}),[C.JNum]:I({0:E}),[C.JStr]:I({0:U}),[C.JArr]:I({0:A(e)}),[C.JObj]:I({0:A(B(U,e))})})));var T;!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.Num=2]=\"Num\",e[e.Str=3]=\"Str\",e[e.Arr=4]=\"Arr\",e[e.Obj=5]=\"Obj\",e[e.Dot=6]=\"Dot\",e[e.AssignProp=7]=\"AssignProp\",e[e.Ix=8]=\"Ix\",e[e.Add=9]=\"Add\",e[e.Subtract=10]=\"Subtract\",e[e.Multiply=11]=\"Multiply\",e[e.Divide=12]=\"Divide\",e[e.Id=13]=\"Id\",e[e.Lam=14]=\"Lam\",e[e.Apply=15]=\"Apply\",e[e.Call=16]=\"Call\",e[e.AssignVar=17]=\"AssignVar\",e[e.FreeVar=18]=\"FreeVar\",e[e.Var=19]=\"Var\",e[e.ElInitBuilder=20]=\"ElInitBuilder\",e[e.ElDestroyBuilder=21]=\"ElDestroyBuilder\",e[e.ElPush=22]=\"ElPush\",e[e.ElNoPush=23]=\"ElNoPush\",e[e.ElProp=24]=\"ElProp\",e[e.ElAttr=25]=\"ElAttr\",e[e.ElEvent=26]=\"ElEvent\",e[e.ElText=27]=\"ElText\",e[e.ElAssignTextContent=28]=\"ElAssignTextContent\",e[e.ElPop=29]=\"ElPop\",e[e.ElInsertBoundary=30]=\"ElInsertBoundary\",e[e.ElClearBoundary=31]=\"ElClearBoundary\",e[e.ElToggleClass=32]=\"ElToggleClass\",e[e.RevSeq=33]=\"RevSeq\",e[e.ExecCallback=34]=\"ExecCallback\",e[e.UncaughtException=35]=\"UncaughtException\"}(T||(T={}));const J=D((e=>k({[T.Null]:I({}),[T.Boolean]:I({0:x}),[T.Num]:I({0:E}),[T.Str]:I({0:U}),[T.Arr]:I({0:A(e)}),[T.Obj]:I({0:A(B(U,e))}),[T.Dot]:I({0:e,1:U}),[T.AssignProp]:I({0:e,1:U,2:e}),[T.Ix]:I({exp:e,ix:E}),[T.Add]:I({0:e,1:e}),[T.Subtract]:I({0:e,1:e}),[T.Multiply]:I({0:e,1:e}),[T.Divide]:I({0:e,1:e}),[T.Id]:I({0:U}),[T.Lam]:I({args:A(U),body:e}),[T.Apply]:I({0:e,1:A(e)}),[T.Call]:I({0:e,1:U,2:A(e)}),[T.AssignVar]:I({lhs:E,rhs:e}),[T.FreeVar]:I({varId:E}),[T.Var]:I({varId:E}),[T.ElInitBuilder]:I({varId:E,element:e}),[T.ElDestroyBuilder]:I({varId:E}),[T.ElPush]:I({varId:E,tagName:U}),[T.ElNoPush]:I({varId:E,tagName:U}),[T.ElProp]:I({varId:E,prop:U,val:e}),[T.ElAttr]:I({varId:E,attr:U,val:U}),[T.ElEvent]:I({varId:E,name:U,callback:e}),[T.ElText]:I({varId:E,content:U}),[T.ElAssignTextContent]:I({varId:E,content:U}),[T.ElPop]:I({varId:E}),[T.ElInsertBoundary]:I({varId:E}),[T.ElClearBoundary]:I({varId:E}),[T.ElToggleClass]:I({varId:E,className:U,enable:x}),[T.RevSeq]:I({exprs:A(e)}),[T.ExecCallback]:I({callbackId:E,arg:e}),[T.UncaughtException]:I({message:U})})));var P;!function(e){e[e.Eval=0]=\"Eval\",e[e.HotReload=1]=\"HotReload\",e[e.Exit=2]=\"Exit\"}(P||(P={}));const j=k({[P.Eval]:I({expr:J}),[P.HotReload]:I({}),[P.Exit]:I({})});var L;!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.ExecCallback=2]=\"ExecCallback\"}(L||(L={}));const O=k({[L.Start]:I({}),[L.Return]:I({0:S}),[L.ExecCallback]:I({arg:S,callbackId:E})}),R=new Map;class z{constructor(e,t){this._parent=e,this._element=t}}class M{constructor(e,t,r){this._parent=e,this._begin=t,this._end=r}}function F(e){const[t,r]=[e._begin,e._end];for(;r.previousSibling&&r.previousSibling.parentNode&&r.previousSibling!==t;)r.previousSibling.parentNode.removeChild(r.previousSibling)}function H(e,t){if(e instanceof z)e._element.appendChild(t);else{if(!(e instanceof M))return l();e._end.parentElement.insertBefore(t,e._end)}}function W(e,t={tag:L.Start}){const r=$(e,t);switch(r.tag){case P.Eval:{const t=N(q,(t=>W(e,t)),r.expr),n=V(t);return W(e,{tag:L.Return,0:n})}case P.HotReload:return void window.location.reload();case P.Exit:return}l()}const q=[window,null];function $(e,t){const r=O.encode(t);console.log(`sending ${r.length} bytes`);const n=function(e,t){const r=t.byteLength,n=e.exports.hs_malloc(t.length+8);return new DataView(e.exports.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.exports.memory.buffer,n+8,r).set(t),n}(e,r),s=function(e,t){const r=new Uint8Array(e.exports.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),s=new Uint8Array(e.exports.memory.buffer,t+8,n).slice().buffer;return e.exports.hs_free(t),new Uint8Array(s)}(e,e.exports.app(n));return console.log(`receiving ${s.length} bytes`),j.decode(s)}var X=function(e,t,r,n){return new(r||(r=Promise))((function(s,i){function a(e){try{o(n.next(e))}catch(e){i(e)}}function l(e){try{o(n.throw(e))}catch(e){i(e)}}function o(e){var t;e.done?s(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(a,l)}o((n=n.apply(e,t||[])).next())}))};const G=[window,null];window.startReactor=async function(r){let n=new class{start(e){this.inst=e,e.exports._start()}initialize(e){this.inst=e,e.exports._initialize()}constructor(r,n,s){this.args=[],this.env=[],this.fds=[],this.args=r,this.env=n,this.fds=s;let i=this;this.wasiImport={args_sizes_get(e,t){let r=new DataView(i.inst.exports.memory.buffer);r.setUint32(e,i.args.length,!0);let n=0;for(let e of i.args)n+=e.length+1;return r.setUint32(t,n,!0),0},args_get(e,t){let r=new DataView(i.inst.exports.memory.buffer),n=new Uint8Array(i.inst.exports.memory.buffer);for(let s=0;s<i.args.length;s++){r.setUint32(e,t,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(i.args[s]);n.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return 0},environ_sizes_get(e,t){let r=new DataView(i.inst.exports.memory.buffer);r.setUint32(e,i.env.length,!0);let n=0;for(let e of i.env)n+=e.length+1;return r.setUint32(t,n,!0),0},environ_get(e,t){let r=new DataView(i.inst.exports.memory.buffer),s=new Uint8Array(i.inst.exports.memory.buffer);for(let i=0;i<n.length;i++){r.setUint32(e,t,!0),e+=4;let a=new TextEncoder(\"utf-8\").encode(n[i]);s.set(a,t),r.setUint8(t+a.length,0),t+=a.length+1}return 0},clock_res_get(e,t){throw\"unimplemented\"},clock_time_get(e,t,r){let n=new DataView(i.inst.exports.memory.buffer);if(0===e)n.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}n.setBigUint64(r,e,!0)}else n.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,n)=>null!=i.fds[e]?i.fds[e].fd_advise(t,r,n):8,fd_allocate:(e,t,r)=>null!=i.fds[e]?i.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=i.fds[e]){let t=i.fds[e].fd_close();return i.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=i.fds[e]?i.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=i.fds[e]){let{ret:r,fdstat:n}=i.fds[e].fd_fdstat_get();return null!=n&&n.write_bytes(new DataView(i.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=i.fds[e]?i.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=i.fds[e]?i.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=i.fds[e]){let{ret:r,filestat:n}=i.fds[e].fd_filestat_get();return null!=n&&n.write_bytes(new DataView(i.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=i.fds[e]?i.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,n)=>null!=i.fds[e]?i.fds[e].fd_filestat_set_times(t,r,n):8,fd_pread(t,r,n,s,a){let l=new DataView(i.inst.exports.memory.buffer),o=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[t]){let f=e.read_bytes_array(l,r,n),{ret:u,nread:d}=i.fds[t].fd_pread(o,f,s);return l.setUint32(a,d,!0),u}return 8},fd_prestat_get(e,t){let r=new DataView(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let{ret:n,prestat:s}=i.fds[e].fd_prestat_get();return null!=s&&s.write_bytes(r,t),n}return 8},fd_prestat_dir_name(e,t,r){if(null!=i.fds[e]){let{ret:r,prestat_dir_name:n}=i.fds[e].fd_prestat_dir_name();return null!=n&&new Uint8Array(i.inst.exports.memory.buffer).set(n,t),r}return 8},fd_pwrite(e,r,n,s,a){let l=new DataView(i.inst.exports.memory.buffer),o=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let f=t.read_bytes_array(l,r,n),{ret:u,nwritten:d}=i.fds[e].fd_pwrite(o,f,s);return l.setUint32(a,d,!0),u}return 8},fd_read(t,r,n,s){let a=new DataView(i.inst.exports.memory.buffer),l=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[t]){let o=e.read_bytes_array(a,r,n),{ret:f,nread:u}=i.fds[t].fd_read(l,o);return a.setUint32(s,u,!0),f}return 8},fd_readdir(e,t,r,n,s){let a=new DataView(i.inst.exports.memory.buffer),l=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let o=0;for(;;){let{ret:f,dirent:u}=i.fds[e].fd_readdir_single(n);if(0!=f)return a.setUint32(s,o,!0),f;if(null==u)break;if(r-o<u.head_length()){o=r;break}let d=new ArrayBuffer(u.head_length());if(u.write_head_bytes(new DataView(d),0),l.set(new Uint8Array(d).slice(0,Math.min(d.byteLength,r-o)),t),t+=u.head_length(),o+=u.head_length(),r-o<u.name_length()){o=r;break}u.write_name_bytes(l,t,r-o),t+=u.name_length(),o+=u.name_length(),n=u.d_next}return a.setUint32(s,o,!0),0}return 8},fd_renumber(e,t){if(null!=i.fds[e]&&null!=i.fds[t]){let r=i.fds[t].fd_close();return 0!=r?r:(i.fds[t]=i.fds[e],i.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,n){let s=new DataView(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let{ret:a,offset:l}=i.fds[e].fd_seek(t,r);return s.setBigInt64(n,l,!0),a}return 8},fd_sync:e=>null!=i.fds[e]?i.fds[e].fd_sync():8,fd_tell(e,t){let r=new DataView(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let{ret:n,offset:s}=i.fds[e].fd_tell();return r.setBigUint64(t,s,!0),n}return 8},fd_write(e,r,n,s){let a=new DataView(i.inst.exports.memory.buffer),l=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let o=t.read_bytes_array(a,r,n),{ret:f,nwritten:u}=i.fds[e].fd_write(l,o);return a.setUint32(s,u,!0),f}return 8},path_create_directory(e,t,r){let n=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let s=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return i.fds[e].path_create_directory(s)}},path_filestat_get(e,t,r,n,s){let a=new DataView(i.inst.exports.memory.buffer),l=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let o=new TextDecoder(\"utf-8\").decode(l.slice(r,r+n)),{ret:f,filestat:u}=i.fds[e].path_filestat_get(t,o);return null!=u&&u.write_bytes(a,s),f}return 8},path_filestat_set_times(e,t,r,n,s,a,l){let o=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let f=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n));return i.fds[e].path_filestat_set_times(t,f,s,a,l)}return 8},path_link(e,t,r,n,s,a,l){let o=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]&&null!=i.fds[s]){let f=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n)),u=new TextDecoder(\"utf-8\").decode(o.slice(a,a+l));return i.fds[s].path_link(e,t,f,u)}return 8},path_open(e,t,r,n,s,a,l,o,f){let u=new DataView(i.inst.exports.memory.buffer),d=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let c=new TextDecoder(\"utf-8\").decode(d.slice(r,r+n)),{ret:_,fd_obj:p}=i.fds[e].path_open(t,c,s,a,l,o);if(0!=_)return _;i.fds.push(p);let h=i.fds.length-1;return u.setUint32(f,h,!0),0}return 8},path_readlink(e,t,r,n,s,a){let l=new DataView(i.inst.exports.memory.buffer),o=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let f=new TextDecoder(\"utf-8\").decode(o.slice(t,t+r)),{ret:u,data:d}=i.fds[e].path_readlink(f);if(null!=d){if(d.length>s)return l.setUint32(a,0,!0),8;o.set(d,n),l.setUint32(a,d.length,!0)}return u}return 8},path_remove_directory(e,t,r){let n=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let s=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return i.fds[e].path_remove_directory(s)}return 8},path_rename(e,t,r,n,s,i){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,n,s){let a=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[r]){let l=new TextDecoder(\"utf-8\").decode(a.slice(e,e+t)),o=new TextDecoder(\"utf-8\").decode(a.slice(n,n+s));return i.fds[r].path_symlink(l,o)}return 8},path_unlink_file(e,t,r){let n=new Uint8Array(i.inst.exports.memory.buffer);if(null!=i.fds[e]){let s=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return i.fds[e].path_unlink_file(s)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw\"exit with exit code \"+e},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){let r=new Uint8Array(i.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"}}}}([],[],[new i(new a([])),new i(new a([])),new i(new a([]))]),s=await WebAssembly.compileStreaming(fetch(\"./build/wasm32-wasi/ghc-9.9.20230916/htmlt-wasm-0.1.0.0/x/voting/build/voting/voting.wasm\")),l=await WebAssembly.instantiate(s,{wasi_snapshot_preview1:n.wasiImport});n.inst=l,l.exports.hs_init(0,0),W(l)},window.startDevClient=function(e){return X(this,void 0,void 0,(function*(){const t=new WebSocket(e);t.onopen=e=>{const r=O.encode({tag:L.Start});t.send(r)},t.onmessage=e=>X(this,void 0,void 0,(function*(){const r=yield(n=e.data,new Promise(((e,t)=>{const r=new FileReader;r.onload=()=>{const t=r.result,n=new Uint8Array(t);e(n)},r.onerror=e=>{t(e)},r.readAsArrayBuffer(n)})));var n;!function(e,t){X(this,void 0,void 0,(function*(){switch(e.tag){case P.Eval:{const r=V(N(G,t,e.expr));return t({tag:L.Return,0:r})}case P.HotReload:return void window.location.reload();case P.Exit:return}l()}))}(j.decode(r),(e=>t.send(O.encode(e))))})),t.onerror=e=>{console.error(\"WebSocket error:\",e)},t.onclose=e=>{console.log(\"WebSocket connection closed:\",e)}}))}})();"
