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
import "this" HtmlT.Wasm.JSM
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
  , init_app :: a -> IO (JSM (), Application)
  , html_template :: BSL.ByteString -> BSL.ByteString
  }

data RunningApp = forall a. Typeable a => RunningApp
  { resource :: a
  , debug_config :: DebugConfig a
  , client_app :: JSM ()
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
            hPutStrLn stderr $ "Already in use, trying next port…"
            tryPort (setPort (getPort settings + 1) settings) application
          | otherwise -> throwIO e

runDebugDefault :: Warp.Port -> JSM () -> IO ()
runDebugDefault port wasmApp =
  runDebug (Warp.setPort port Warp.defaultSettings) DebugConfig
    { open_resource = pure ()
    , close_resource = const (pure ())
    , init_app = const $ pure (wasmApp, defaultFallbackApp)
    , html_template = defaultHtmlTemplate
    }

devserverMiddleware :: DevServerInstance -> Middleware
devserverMiddleware opts next req resp =
  case pathInfo req of
    [] -> indexHtmlApp req resp
    ["index.html"] -> indexHtmlApp req resp
    ["dev-server.sock"] -> devserverApp req resp
    _ -> next req resp
  where
    devserverApp =
      websocketsOr defaultConnectionOptions (devserverWebsocket opts) defaultFallbackApp
    indexHtmlApp req resp = do
      let origin = inferOrigin req
      RunningApp{debug_config} <- readIORef opts.app_state_ref
      resp $ responseLBS status200
        [(hContentType, "text/html; charset=utf-8")] $
        debug_config.html_template (BSL.fromStrict origin)
    inferOrigin req = WAI.requestHeaders req
      & List.lookup "Host"
      & fromMaybe "localhost"
      & ((if WAI.isSecure req then "wss://" else "ws://") <>)

defaultHtmlTemplate :: BSL.ByteString -> BSL.ByteString
defaultHtmlTemplate origin =
  "<html>\n\
  \ <body>\n\
  \  <script>\n\
  \    " <> BSL.fromStrict indexBundleJs <> "\n\
  \    startDevClient(\"" <> origin <> "/dev-server.sock\");\n\
  \  </script>\n\
  \ </body>\n\
  \</html>\n\
  \"

defaultFallbackApp :: Application
defaultFallbackApp _ resp =
  resp $ responseLBS status404
    [(hContentType, "text/html; charset=utf-8")]
    "<html>\n\
    \ <body>\n\
    \   <h1>Not Found</h1>\n\
    \ </body>\n\
    \</html>\n\
    \"

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

-- | Run @yarn run webpack --mode production@ and copy contents here
-- from @./dist-newstyle/index.bundle.js@
indexBundleJs :: ByteString
indexBundleJs = "(()=>{\"use strict\";var __webpack_modules__={268:(e,t,r)=>{r.d(t,{BE:()=>w,IM:()=>T,IX:()=>M,Z_:()=>m,a0:()=>D,a2:()=>y,bc:()=>v,cS:()=>h});var n=r(582),_=r(849),a=function(){function e(){}return e.prototype.encode=function(e){var t=E(this,e),r=new Uint8Array(t);return b(this,r,0,e),r},e.prototype.decode=function(e){var t=x(this,e,0),r=t[0];return t[1],r},e}(),i=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return(0,n.ZT)(t,e),t}(a),s=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return(0,n.ZT)(t,e),t}(a),o=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return(0,n.ZT)(t,e),t}(a),l=function(e){function t(){return null!==e&&e.apply(this,arguments)||this}return(0,n.ZT)(t,e),t}(a),u=function(e){function t(t){var r=e.call(this)||this;return r._element=t,r}return(0,n.ZT)(t,e),t}(a),f=function(e){function t(t){var r=e.call(this)||this;return r._description=t,r}return(0,n.ZT)(t,e),t}(a),c=function(e){function t(t){var r=e.call(this)||this;return r._alternatives=t,r}return(0,n.ZT)(t,e),t}(a),d=function(e){function t(t){var r=e.call(this)||this;return r._self=t,r}return(0,n.ZT)(t,e),t}(a),p=function(e){function t(t){var r=e.call(this)||this;return r._tuple=t,r}return(0,n.ZT)(t,e),t}(a);function E(e,t){if(e instanceof i)return 1;if(e instanceof s)return 8;if(e instanceof l){var r=t;return(n=8)+(new TextEncoder).encode(r).length}if(e instanceof o)return(n=8)+t.length;if(e instanceof u){var n=8;return t.reduce((function(t,r){return t+E(e._element,r)}),n)}if(e instanceof f){var a=t;return Object.keys(e._description).reduce((function(t,r){return t+E(e._description[r],a[r])}),0)}if(e instanceof c){var x=t;return g(Object.keys(e._alternatives).length)+E(e._alternatives[x.tag],x)}if(e instanceof d)return E(e._self,t);if(e instanceof p){var b=t;return e._tuple.reduce((function(e,t,r){return e+E(t,b[r])}),0)}return(0,_.R)(e)}function x(e,t,r){if(e instanceof i)return[t[r],r+1];if(e instanceof s)return[m=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),r+8];if(e instanceof l){var n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),a=t.subarray(r+8,r+8+n);return[new TextDecoder(\"utf8\").decode(a),r+8+n]}if(e instanceof o)return n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56),[t.subarray(r+8,r+8+n),r+8+n];if(e instanceof u){n=t[r+7]+(t[r+6]<<8)+(t[r+5]<<16)+(t[r+4]<<24)+(t[r+3]<<32)+(t[r+2]<<40)+(t[r+1]<<48)+(t[r]<<56);for(var E=[],b=r+8,h=0;h<n;h++){var y=x(e._element,t,b),m=y[0],M=y[1];E.push(m),b=M}return[E,b]}if(e instanceof f){var T=r,D=Object.fromEntries(Object.entries(e._description).map((function(e){var r=e[0],n=x(e[1],t,T),_=n[0],a=n[1];return T=a,[r,_]})));return[D,T]}if(e instanceof c){var v=function(e,t,r){if(1!=e)throw new Error(\"Unimplemented\");return[t[r],r+1]}(g(Object.keys(e._alternatives).length),t,r),w=v[0],O=v[1],P=x(e._alternatives[w],t,O),C=P[0],I=P[1];return C.tag=w,[C,I]}if(e instanceof d)return x(e._self,t,r);if(e instanceof p){var A=r,U=e._tuple.map((function(e){var r=x(e,t,A),n=r[0],_=r[1];return A=_,n}));return[U,A]}return(0,_.R)(e)}function b(e,t,r,n){if(e instanceof i)return t[r]=n,r+1;if(e instanceof s){var a=n;return t[r+7]=255&a,t[r+6]=a>>8&255,t[r+5]=a>>16&255,t[r+4]=a>>24&255,r+8}if(e instanceof l){var E=n,x=(new TextEncoder).encode(E),h=x.length;return t[r+7]=255&h,t[r+6]=h>>8&255,t[r+5]=h>>16&255,t[r+4]=h>>24&255,t.set(x,r+8),r+8+h}if(e instanceof o){var y=n;return h=y.length,t[r+7]=255&h,t[r+6]=h>>8&255,t[r+5]=h>>16&255,t[r+4]=h>>24&255,t.set(y,r+8),r+8+h}if(e instanceof u){var m=n;h=m.length,t[r+7]=255&h,t[r+6]=h>>8&255,t[r+5]=h>>16&255,t[r+4]=h>>24&255;for(var M=r+8,T=0;T<h;T++)M=b(e._element,t,M,m[T]);return M}if(e instanceof f){var D=n;for(var v in M=r,e._description)Object.prototype.hasOwnProperty.call(e._description,v)&&(M=b(e._description[v],t,M,D[v]));return M}if(e instanceof c){var w=n.tag,O=g(Object.keys(e._alternatives).length);return t[r]=w,b(e._alternatives[w],t,r+O,n)}if(e instanceof d)return b(e._self,t,r,n);if(e instanceof p){var P=n,C=r;return e._tuple.forEach((function(e,r){C=b(e,t,C,P[r])})),C}return(0,_.R)(e)}function g(e){return Math.ceil(Math.log2(e)/8)}var h=new i,y=new s,m=new l;function M(e){return new u(e)}function T(e){return new f(e)}function D(e){return new c(e)}function v(){for(var e=[],t=0;t<arguments.length;t++)e[t]=arguments[t];return new p(e)}function w(e){var t=new d(void 0),r=e(t);return t._self=r,r}new o},849:(e,t,r)=>{function n(e){throw new Error(\"absurd: unreachable code\")}r.d(t,{R:()=>n})},741:(__unused_webpack_module,__webpack_exports__,__webpack_require__)=>{__webpack_require__.d(__webpack_exports__,{Cs:()=>upCmd,Xq:()=>unknownToJValue,bs:()=>DownCmdTag,sN:()=>evalExpr,ur:()=>UpCommandTag,y:()=>downCmd});var _binary__WEBPACK_IMPORTED_MODULE_0__=__webpack_require__(268),_lib__WEBPACK_IMPORTED_MODULE_1__=__webpack_require__(849),_a,_b,JValueTag;function Cons(e,t){return[e,t]}function car(e){return e[0]}function cdr(e){return e[1]}function evalExpr(ctx,argCtx,hscb,exp){switch(exp.tag){case ExprTag.Null:return null;case ExprTag.Boolean:return 0!=exp[0];case ExprTag.Num:return exp.coefficient*Math.pow(10,exp.base10Exponent);case ExprTag.Str:return exp[0];case ExprTag.Arr:return exp[0].map(evalExpr.bind(void 0,ctx,argCtx,hscb));case ExprTag.Obj:return Object.fromEntries(exp[0].map((function(e){var t=e[0],r=e[1];return[t,evalExpr(ctx,argCtx,hscb,r)]})));case ExprTag.Dot:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]);return lhs[exp[1]];case ExprTag.AssignProp:var rhs=evalExpr(ctx,argCtx,hscb,exp[2]),obj=evalExpr(ctx,argCtx,hscb,exp[0]);return obj[exp[1]]=rhs,rhs;case ExprTag.Ix:var rhs=evalExpr(ctx,argCtx,hscb,exp.exp);return rhs[exp.ix];case ExprTag.Add:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]),rhs=evalExpr(ctx,argCtx,hscb,exp[1]);return lhs+rhs;case ExprTag.Subtract:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]),rhs=evalExpr(ctx,argCtx,hscb,exp[1]);return lhs-rhs;case ExprTag.Multiply:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]),rhs=evalExpr(ctx,argCtx,hscb,exp[1]);return lhs*rhs;case ExprTag.Divide:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]),rhs=evalExpr(ctx,argCtx,hscb,exp[1]);return lhs/rhs;case ExprTag.Id:for(var ident=exp[0],iter=ctx;iter;iter=cdr(iter)){var bindings=car(iter);if(ident in bindings)return bindings[ident]}throw new Error(\"Variable not in scope: \"+exp[0]);case ExprTag.Lam:return function(){return evalExpr(ctx,Cons(arguments,argCtx),hscb,exp.body)};case ExprTag.Arg:for(var iter=argCtx,j=0;iter;){if(j==exp.scopeIx){var iarguments=car(iter);return iarguments[j]}iter=cdr(iter),j++}throw new Error(\"Argument scope out of a rabge: \"+exp.scopeIx);case ExprTag.Apply:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]);return lhs.apply(void 0,exp[1].map(evalExpr.bind(void 0,ctx,argCtx,hscb)));case ExprTag.Call:var lhs=evalExpr(ctx,argCtx,hscb,exp[0]),fn=lhs[exp[1]];return fn.apply(lhs,exp[2].map(evalExpr.bind(void 0,ctx,argCtx,hscb)));case ExprTag.AssignVar:var rhs=evalExpr(ctx,argCtx,hscb,exp.rhs);return varStorage.set(exp.lhs,rhs),rhs;case ExprTag.FreeVar:return varStorage.delete(exp.varId);case ExprTag.Var:return varStorage.get(exp.varId);case ExprTag.InsertNode:var parent_1=evalExpr(ctx,argCtx,hscb,exp.parent),child=evalExpr(ctx,argCtx,hscb,exp.child);return domBuilder.insertIntoBuilder(parent_1,child),null;case ExprTag.WithBuilder:var builder=evalExpr(ctx,argCtx,hscb,exp.builder),builderContent=evalExpr(ctx,argCtx,hscb,exp.builderContent);return builderContent(builder),builder;case ExprTag.CreateElement:return document.createElement(exp.tagName);case ExprTag.CreateText:return document.createTextNode(exp.content);case ExprTag.ElementProp:var parent_2=evalExpr(ctx,argCtx,hscb,exp.node),propValue=evalExpr(ctx,argCtx,hscb,exp.propValue);return domBuilder.assignProperty(parent_2,exp.propName,propValue),null;case ExprTag.ElementAttr:var parent_3=evalExpr(ctx,argCtx,hscb,exp.node);return domBuilder.assignAttribute(parent_3,exp.attrName,exp.attrValue),null;case ExprTag.AddEventListener:var parent_4=evalExpr(ctx,argCtx,hscb,exp.node),listener=evalExpr(ctx,argCtx,hscb,exp.listener);return domBuilder.addEventListener(parent_4,exp.eventName,listener),null;case ExprTag.ToggleClass:var parent_5=evalExpr(ctx,argCtx,hscb,exp.node);return domBuilder.toggleClass(parent_5,exp.className,Boolean(exp.enable)),null;case ExprTag.AssignText:var node=evalExpr(ctx,argCtx,hscb,exp.node);return node.textContent=exp.content,null;case ExprTag.InsertBoundary:var parent_6=evalExpr(ctx,argCtx,hscb,exp.parent);return domBuilder.insertBoundary(parent_6);case ExprTag.ClearBoundary:var boundary=evalExpr(ctx,argCtx,hscb,exp.boundary);return domBuilder.clearBoundary(boundary,Boolean(exp.detach));case ExprTag.RevSeq:return exp.exprs.reduceRight((function(e,t){return evalExpr(ctx,argCtx,hscb,t)}),null);case ExprTag.Eval:return eval(exp.rawJavaScript);case ExprTag.ExecCallback:var arg=evalExpr(ctx,argCtx,hscb,exp.arg);return hscb({tag:DownCmdTag.ExecCallback,arg:unknownToJValue(arg),callbackId:exp.callbackId});case ExprTag.UncaughtException:throw new Error(exp.message)}(0,_lib__WEBPACK_IMPORTED_MODULE_1__.R)(exp)}function unknownToJValue(e){if(\"boolean\"==typeof e)return{tag:JValueTag.JBool,0:e?1:0};if(\"number\"==typeof e){var t=toScientific(e),r=t.coefficient,n=t.base10Exponent;return{tag:JValueTag.JNum,coefficient:r,base10Exponent:n}}if(\"string\"==typeof e)return{tag:JValueTag.JStr,0:e};if(Array.isArray(e))return{tag:JValueTag.JArr,0:e.map(unknownToJValue)};if(null==e)return{tag:JValueTag.JNull};var _=Object.entries(e).map((function(e){return[e[0],unknownToJValue(e[1])]}));return{tag:JValueTag.JObj,0:_}}function toScientific(e){if(0===e)return{coefficient:0,base10Exponent:0};for(var t=0,r=e;r%10!=0;)r*=10,t--;return{coefficient:r,base10Exponent:t}}!function(e){e[e.JNull=0]=\"JNull\",e[e.JBool=1]=\"JBool\",e[e.JNum=2]=\"JNum\",e[e.JStr=3]=\"JStr\",e[e.JArr=4]=\"JArr\",e[e.JObj=5]=\"JObj\"}(JValueTag||(JValueTag={}));var jvalue=_binary__WEBPACK_IMPORTED_MODULE_0__.BE((function(e){var t;return _binary__WEBPACK_IMPORTED_MODULE_0__.a0(((t={})[JValueTag.JNull]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({}),t[JValueTag.JBool]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[JValueTag.JNum]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({coefficient:_binary__WEBPACK_IMPORTED_MODULE_0__.a2,base10Exponent:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[JValueTag.JStr]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[JValueTag.JArr]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(e)}),t[JValueTag.JObj]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(_binary__WEBPACK_IMPORTED_MODULE_0__.bc(_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,e))}),t))})),ExprTag;!function(e){e[e.Null=0]=\"Null\",e[e.Boolean=1]=\"Boolean\",e[e.Num=2]=\"Num\",e[e.Str=3]=\"Str\",e[e.Arr=4]=\"Arr\",e[e.Obj=5]=\"Obj\",e[e.Dot=6]=\"Dot\",e[e.AssignProp=7]=\"AssignProp\",e[e.Ix=8]=\"Ix\",e[e.Add=9]=\"Add\",e[e.Subtract=10]=\"Subtract\",e[e.Multiply=11]=\"Multiply\",e[e.Divide=12]=\"Divide\",e[e.Id=13]=\"Id\",e[e.Lam=14]=\"Lam\",e[e.Arg=15]=\"Arg\",e[e.Apply=16]=\"Apply\",e[e.Call=17]=\"Call\",e[e.AssignVar=18]=\"AssignVar\",e[e.FreeVar=19]=\"FreeVar\",e[e.Var=20]=\"Var\",e[e.InsertNode=21]=\"InsertNode\",e[e.WithBuilder=22]=\"WithBuilder\",e[e.CreateElement=23]=\"CreateElement\",e[e.CreateText=24]=\"CreateText\",e[e.ElementProp=25]=\"ElementProp\",e[e.ElementAttr=26]=\"ElementAttr\",e[e.AddEventListener=27]=\"AddEventListener\",e[e.ToggleClass=28]=\"ToggleClass\",e[e.AssignText=29]=\"AssignText\",e[e.InsertBoundary=30]=\"InsertBoundary\",e[e.ClearBoundary=31]=\"ClearBoundary\",e[e.RevSeq=32]=\"RevSeq\",e[e.Eval=33]=\"Eval\",e[e.ExecCallback=34]=\"ExecCallback\",e[e.UncaughtException=35]=\"UncaughtException\"}(ExprTag||(ExprTag={}));var expr=_binary__WEBPACK_IMPORTED_MODULE_0__.BE((function(e){var t;return _binary__WEBPACK_IMPORTED_MODULE_0__.a0(((t={})[ExprTag.Null]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({}),t[ExprTag.Boolean]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[ExprTag.Num]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({coefficient:_binary__WEBPACK_IMPORTED_MODULE_0__.a2,base10Exponent:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[ExprTag.Str]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.Arr]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(e)}),t[ExprTag.Obj]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(_binary__WEBPACK_IMPORTED_MODULE_0__.bc(_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,e))}),t[ExprTag.Dot]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.AssignProp]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,2:e}),t[ExprTag.Ix]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({exp:e,ix:_binary__WEBPACK_IMPORTED_MODULE_0__.a2}),t[ExprTag.Add]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:e}),t[ExprTag.Subtract]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:e}),t[ExprTag.Multiply]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:e}),t[ExprTag.Divide]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:e}),t[ExprTag.Id]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.Lam]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({body:e}),t[ExprTag.Arg]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({scopeIx:_binary__WEBPACK_IMPORTED_MODULE_0__.cS,argIx:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[ExprTag.Apply]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(e)}),t[ExprTag.Call]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:e,1:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,2:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(e)}),t[ExprTag.AssignVar]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({lhs:_binary__WEBPACK_IMPORTED_MODULE_0__.a2,rhs:e}),t[ExprTag.FreeVar]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({varId:_binary__WEBPACK_IMPORTED_MODULE_0__.a2}),t[ExprTag.Var]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({varId:_binary__WEBPACK_IMPORTED_MODULE_0__.a2}),t[ExprTag.InsertNode]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({parent:e,child:e}),t[ExprTag.WithBuilder]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({builder:e,builderContent:e}),t[ExprTag.CreateElement]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({tagName:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.CreateText]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({content:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.ElementProp]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({node:e,propName:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,propValue:e}),t[ExprTag.ElementAttr]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({node:e,attrName:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,attrValue:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.AddEventListener]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({node:e,eventName:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,listener:e}),t[ExprTag.ToggleClass]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({node:e,className:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_,enable:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[ExprTag.AssignText]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({node:e,content:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.InsertBoundary]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({parent:e}),t[ExprTag.ClearBoundary]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({boundary:e,detach:_binary__WEBPACK_IMPORTED_MODULE_0__.cS}),t[ExprTag.RevSeq]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({exprs:_binary__WEBPACK_IMPORTED_MODULE_0__.IX(e)}),t[ExprTag.Eval]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({rawJavaScript:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t[ExprTag.ExecCallback]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.a2,arg:e}),t[ExprTag.UncaughtException]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({message:_binary__WEBPACK_IMPORTED_MODULE_0__.Z_}),t))})),UpCommandTag;!function(e){e[e.EvalExpr=0]=\"EvalExpr\",e[e.HotReload=1]=\"HotReload\",e[e.Exit=2]=\"Exit\"}(UpCommandTag||(UpCommandTag={}));var upCmd=_binary__WEBPACK_IMPORTED_MODULE_0__.a0((_a={},_a[UpCommandTag.EvalExpr]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({expr}),_a[UpCommandTag.HotReload]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({}),_a[UpCommandTag.Exit]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({}),_a)),DownCmdTag;!function(e){e[e.Start=0]=\"Start\",e[e.Return=1]=\"Return\",e[e.ExecCallback=2]=\"ExecCallback\"}(DownCmdTag||(DownCmdTag={}));var downCmd=_binary__WEBPACK_IMPORTED_MODULE_0__.a0((_b={},_b[DownCmdTag.Start]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({}),_b[DownCmdTag.Return]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({0:jvalue}),_b[DownCmdTag.ExecCallback]=_binary__WEBPACK_IMPORTED_MODULE_0__.IM({arg:jvalue,callbackId:_binary__WEBPACK_IMPORTED_MODULE_0__.a2}),_b)),varStorage=new Map,domBuilder;!function(e){function t(e,t){e instanceof Comment?e.parentElement.insertBefore(t,e):e.appendChild(t)}function r(e){return e instanceof Comment?e.parentElement:e}function n(e){return e instanceof Comment&&\"ContentBoundary {{\"==e.textContent}e.insertIntoBuilder=t,e.assignProperty=function(e,t,r){e instanceof Comment?e.parentElement[t]=r:e[t]=r},e.assignAttribute=function(e,t,n){r(e).setAttribute(t,n)},e.addEventListener=function(e,t,n){r(e).addEventListener(t,n)},e.toggleClass=function(e,t,n){var _=r(e);n?_.classList.add(t):_.classList.remove(t)},e.insertBoundary=function(e){var r=document.createComment(\"ContentBoundary {{\"),n=document.createComment(\"}}\");return t(e,r),t(e,n),n},e.clearBoundary=function(e,t){for(var r=e,_=0;r.previousSibling&&(0!=_||!n(r.previousSibling));)(a=r.previousSibling)instanceof Comment&&\"}}\"==a.textContent?_++:n(r.previousSibling)&&_--,r.previousSibling.parentNode.removeChild(r.previousSibling);var a;t&&(r.previousSibling.parentNode.removeChild(r.previousSibling),r.parentNode.removeChild(r))}}(domBuilder||(domBuilder={}))},582:(e,t,r)=>{r.d(t,{Jh:()=>i,ZT:()=>_,mG:()=>a});var n=function(e,t){return n=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(e,t){e.__proto__=t}||function(e,t){for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])},n(e,t)};function _(e,t){if(\"function\"!=typeof t&&null!==t)throw new TypeError(\"Class extends value \"+String(t)+\" is not a constructor or null\");function r(){this.constructor=e}n(e,t),e.prototype=null===t?Object.create(t):(r.prototype=t.prototype,new r)}function a(e,t,r,n){return new(r||(r=Promise))((function(_,a){function i(e){try{o(n.next(e))}catch(e){a(e)}}function s(e){try{o(n.throw(e))}catch(e){a(e)}}function o(e){var t;e.done?_(e.value):(t=e.value,t instanceof r?t:new r((function(e){e(t)}))).then(i,s)}o((n=n.apply(e,t||[])).next())}))}function i(e,t){var r,n,_,a,i={label:0,sent:function(){if(1&_[0])throw _[1];return _[1]},trys:[],ops:[]};return a={next:s(0),throw:s(1),return:s(2)},\"function\"==typeof Symbol&&(a[Symbol.iterator]=function(){return this}),a;function s(s){return function(o){return function(s){if(r)throw new TypeError(\"Generator is already executing.\");for(;a&&(a=0,s[0]&&(i=0)),i;)try{if(r=1,n&&(_=2&s[0]?n.return:s[0]?n.throw||((_=n.return)&&_.call(n),0):n.next)&&!(_=_.call(n,s[1])).done)return _;switch(n=0,_&&(s=[2&s[0],_.value]),s[0]){case 0:case 1:_=s;break;case 4:return i.label++,{value:s[1],done:!1};case 5:i.label++,n=s[1],s=[0];continue;case 7:s=i.ops.pop(),i.trys.pop();continue;default:if(!((_=(_=i.trys).length>0&&_[_.length-1])||6!==s[0]&&2!==s[0])){i=0;continue}if(3===s[0]&&(!_||s[1]>_[0]&&s[1]<_[3])){i.label=s[1];break}if(6===s[0]&&i.label<_[1]){i.label=_[1],_=s;break}if(_&&i.label<_[2]){i.label=_[2],i.ops.push(s);break}_[2]&&i.ops.pop(),i.trys.pop();continue}s=t.call(e,i)}catch(e){s=[6,e],n=0}finally{r=_=0}if(5&s[0])throw s[1];return{value:s[0]?s[1]:void 0,done:!0}}([s,o])}}}Object.create,Object.create,\"function\"==typeof SuppressedError&&SuppressedError}},__webpack_module_cache__={};function __webpack_require__(e){var t=__webpack_module_cache__[e];if(void 0!==t)return t.exports;var r=__webpack_module_cache__[e]={exports:{}};return __webpack_modules__[e](r,r.exports,__webpack_require__),r.exports}__webpack_require__.d=(e,t)=>{for(var r in t)__webpack_require__.o(t,r)&&!__webpack_require__.o(e,r)&&Object.defineProperty(e,r,{enumerable:!0,get:t[r]})},__webpack_require__.o=(e,t)=>Object.prototype.hasOwnProperty.call(e,t);var __webpack_exports__={};(()=>{var e=__webpack_require__(582);class t{static read_bytes(e,r){let n=new t;return n.buf=e.getUint32(r,!0),n.buf_len=e.getUint32(r+4,!0),n}static read_bytes_array(e,r,n){let _=[];for(let a=0;a<n;a++)_.push(t.read_bytes(e,r+8*a));return _}}class r{static read_bytes(e,t){let n=new r;return n.buf=e.getUint32(t,!0),n.buf_len=e.getUint32(t+4,!0),n}static read_bytes_array(e,t,n){let _=[];for(let a=0;a<n;a++)_.push(r.read_bytes(e,t+8*a));return _}}class n{write_bytes(e,t){e.setUint8(t,this.fs_filetype),e.setUint16(t+2,this.fs_flags,!0),e.setBigUint64(t+8,this.fs_rights_base,!0),e.setBigUint64(t+16,this.fs_rights_inherited,!0)}constructor(e,t){this.fs_rights_base=0n,this.fs_rights_inherited=0n,this.fs_filetype=e,this.fs_flags=t}}class _{write_bytes(e,t){e.setBigUint64(t,this.dev,!0),e.setBigUint64(t+8,this.ino,!0),e.setUint8(t+16,this.filetype),e.setBigUint64(t+24,this.nlink,!0),e.setBigUint64(t+32,this.size,!0),e.setBigUint64(t+38,this.atim,!0),e.setBigUint64(t+46,this.mtim,!0),e.setBigUint64(t+52,this.ctim,!0)}constructor(e,t){this.dev=0n,this.ino=0n,this.nlink=0n,this.atim=0n,this.mtim=0n,this.ctim=0n,this.filetype=e,this.size=t}}class a{fd_advise(e,t,r){return-1}fd_allocate(e,t){return-1}fd_close(){return 0}fd_datasync(){return-1}fd_fdstat_get(){return{ret:-1,fdstat:null}}fd_fdstat_set_flags(e){return-1}fd_fdstat_set_rights(e,t){return-1}fd_filestat_get(){return{ret:-1,filestat:null}}fd_filestat_set_size(e){return-1}fd_filestat_set_times(e,t,r){return-1}fd_pread(e,t,r){return{ret:-1,nread:0}}fd_prestat_get(){return{ret:-1,prestat:null}}fd_prestat_dir_name(e,t){return{ret:-1,prestat_dir_name:null}}fd_pwrite(e,t,r){return{ret:-1,nwritten:0}}fd_read(e,t){return{ret:-1,nread:0}}fd_readdir_single(e){return{ret:-1,dirent:null}}fd_seek(e,t){return{ret:-1,offset:0n}}fd_sync(){return 0}fd_tell(){return{ret:-1,offset:0n}}fd_write(e,t){return{ret:-1,nwritten:0}}path_create_directory(e){return-1}path_filestat_get(e,t){return{ret:-1,filestat:null}}path_filestat_set_times(e,t,r,n,_){return-1}path_link(e,t,r,n){return-1}path_open(e,t,r,n,_,a){return{ret:-1,fd_obj:null}}path_readlink(e){return{ret:-1,data:null}}path_remove_directory(e){return-1}path_rename(e,t,r){return-1}path_symlink(e,t){return-1}path_unlink_file(e){return-1}}class i extends a{fd_fdstat_get(){return{ret:0,fdstat:new n(4,0)}}fd_read(e,t){let r=0;for(let n of t){if(!(this.file_pos<this.file.data.byteLength))break;{let t=this.file.data.slice(Number(this.file_pos),Number(this.file_pos+BigInt(n.buf_len)));e.set(t,n.buf),this.file_pos+=BigInt(t.length),r+=t.length}}return{ret:0,nread:r}}fd_seek(e,t){let r;switch(t){case 0:r=e;break;case 1:r=this.file_pos+e;break;case 2:r=BigInt(this.file.data.byteLength)+e;break;default:return{ret:28,offset:0n}}return r<0?{ret:28,offset:0n}:(this.file_pos=r,{ret:0,offset:this.file_pos})}fd_write(e,t){let r=0;if(this.file.readonly)return{ret:8,nwritten:r};for(let n of t){let t=e.slice(n.buf,n.buf+n.buf_len);if(this.file_pos+BigInt(t.byteLength)>this.file.size){let e=this.file.data;this.file.data=new Uint8Array(Number(this.file_pos+BigInt(t.byteLength))),this.file.data.set(e)}this.file.data.set(t.slice(0,Number(this.file.size-this.file_pos)),Number(this.file_pos)),this.file_pos+=BigInt(t.byteLength),r+=n.buf_len}return{ret:0,nwritten:r}}fd_filestat_get(){return{ret:0,filestat:this.file.stat()}}constructor(e){super(),this.file_pos=0n,this.file=e}}class s{open(e){let t=new i(this);return 1&e&&t.fd_seek(0n,2),t}get size(){return BigInt(this.data.byteLength)}stat(){return new _(4,this.size)}truncate(){return this.readonly?63:(this.data=new Uint8Array([]),0)}constructor(e,t){this.data=new Uint8Array(e),this.readonly=!!t?.readonly}}var o=__webpack_require__(849),l=__webpack_require__(741);function u(e,t){void 0===t&&(t={tag:l.bs.Start});var r=function(e,t){var r=function(e,t){var r=t.byteLength,n=e.exports.hs_malloc(t.length+8);return new DataView(e.exports.memory.buffer).setUint32(n,r,!0),new Uint8Array(e.exports.memory.buffer,n+8,r).set(t),n}(e,l.y.encode(t)),n=function(e,t){var r=new Uint8Array(e.exports.memory.buffer,t),n=r[0]+(r[1]<<8)+(r[2]<<16)+(r[3]<<24)+(r[4]<<32)+(r[5]<<40)+(r[6]<<48)+(r[7]<<56),_=new Uint8Array(e.exports.memory.buffer,t+8,n).slice().buffer;return e.exports.hs_free(t),new Uint8Array(_)}(e,e.exports.app(r));return l.Cs.decode(n)}(e,t);switch(r.tag){case l.ur.EvalExpr:var n=l.sN(f,null,(function(t){return u(e,t)}),r.expr),_=l.Xq(n);return u(e,{tag:l.bs.Return,0:_});case l.ur.HotReload:return void window.location.reload();case l.ur.Exit:return}(0,o.R)(r)}var f=[window,null],c=[window,null];window.startReactor=function(n){return(0,e.mG)(this,void 0,void 0,(function(){var _,a,o;return(0,e.Jh)(this,(function(e){switch(e.label){case 0:return _=new class{start(e){this.inst=e,e.exports._start()}initialize(e){this.inst=e,e.exports._initialize()}constructor(e,n,_){this.args=[],this.env=[],this.fds=[],this.args=e,this.env=n,this.fds=_;let a=this;this.wasiImport={args_sizes_get(e,t){let r=new DataView(a.inst.exports.memory.buffer);r.setUint32(e,a.args.length,!0);let n=0;for(let e of a.args)n+=e.length+1;return r.setUint32(t,n,!0),0},args_get(e,t){let r=new DataView(a.inst.exports.memory.buffer),n=new Uint8Array(a.inst.exports.memory.buffer);for(let _=0;_<a.args.length;_++){r.setUint32(e,t,!0),e+=4;let i=new TextEncoder(\"utf-8\").encode(a.args[_]);n.set(i,t),r.setUint8(t+i.length,0),t+=i.length+1}return 0},environ_sizes_get(e,t){let r=new DataView(a.inst.exports.memory.buffer);r.setUint32(e,a.env.length,!0);let n=0;for(let e of a.env)n+=e.length+1;return r.setUint32(t,n,!0),0},environ_get(e,t){let r=new DataView(a.inst.exports.memory.buffer),_=new Uint8Array(a.inst.exports.memory.buffer);for(let a=0;a<n.length;a++){r.setUint32(e,t,!0),e+=4;let i=new TextEncoder(\"utf-8\").encode(n[a]);_.set(i,t),r.setUint8(t+i.length,0),t+=i.length+1}return 0},clock_res_get(e,t){throw\"unimplemented\"},clock_time_get(e,t,r){let n=new DataView(a.inst.exports.memory.buffer);if(0===e)n.setBigUint64(r,1000000n*BigInt((new Date).getTime()),!0);else if(1==e){let e;try{e=BigInt(Math.round(1e6*performance.now()))}catch(t){e=0n}n.setBigUint64(r,e,!0)}else n.setBigUint64(r,0n,!0);return 0},fd_advise:(e,t,r,n)=>null!=a.fds[e]?a.fds[e].fd_advise(t,r,n):8,fd_allocate:(e,t,r)=>null!=a.fds[e]?a.fds[e].fd_allocate(t,r):8,fd_close(e){if(null!=a.fds[e]){let t=a.fds[e].fd_close();return a.fds[e]=void 0,t}return 8},fd_datasync:e=>null!=a.fds[e]?a.fds[e].fd_datasync():8,fd_fdstat_get(e,t){if(null!=a.fds[e]){let{ret:r,fdstat:n}=a.fds[e].fd_fdstat_get();return null!=n&&n.write_bytes(new DataView(a.inst.exports.memory.buffer),t),r}return 8},fd_fdstat_set_flags:(e,t)=>null!=a.fds[e]?a.fds[e].fd_fdstat_set_flags(t):8,fd_fdstat_set_rights:(e,t,r)=>null!=a.fds[e]?a.fds[e].fd_fdstat_set_rights(t,r):8,fd_filestat_get(e,t){if(null!=a.fds[e]){let{ret:r,filestat:n}=a.fds[e].fd_filestat_get();return null!=n&&n.write_bytes(new DataView(a.inst.exports.memory.buffer),t),r}return 8},fd_filestat_set_size:(e,t)=>null!=a.fds[e]?a.fds[e].fd_filestat_set_size(t):8,fd_filestat_set_times:(e,t,r,n)=>null!=a.fds[e]?a.fds[e].fd_filestat_set_times(t,r,n):8,fd_pread(e,r,n,_,i){let s=new DataView(a.inst.exports.memory.buffer),o=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let l=t.read_bytes_array(s,r,n),{ret:u,nread:f}=a.fds[e].fd_pread(o,l,_);return s.setUint32(i,f,!0),u}return 8},fd_prestat_get(e,t){let r=new DataView(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let{ret:n,prestat:_}=a.fds[e].fd_prestat_get();return null!=_&&_.write_bytes(r,t),n}return 8},fd_prestat_dir_name(e,t,r){if(null!=a.fds[e]){let{ret:r,prestat_dir_name:n}=a.fds[e].fd_prestat_dir_name();return null!=n&&new Uint8Array(a.inst.exports.memory.buffer).set(n,t),r}return 8},fd_pwrite(e,t,n,_,i){let s=new DataView(a.inst.exports.memory.buffer),o=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let l=r.read_bytes_array(s,t,n),{ret:u,nwritten:f}=a.fds[e].fd_pwrite(o,l,_);return s.setUint32(i,f,!0),u}return 8},fd_read(e,r,n,_){let i=new DataView(a.inst.exports.memory.buffer),s=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let o=t.read_bytes_array(i,r,n),{ret:l,nread:u}=a.fds[e].fd_read(s,o);return i.setUint32(_,u,!0),l}return 8},fd_readdir(e,t,r,n,_){let i=new DataView(a.inst.exports.memory.buffer),s=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let o=0;for(;;){let{ret:l,dirent:u}=a.fds[e].fd_readdir_single(n);if(0!=l)return i.setUint32(_,o,!0),l;if(null==u)break;if(r-o<u.head_length()){o=r;break}let f=new ArrayBuffer(u.head_length());if(u.write_head_bytes(new DataView(f),0),s.set(new Uint8Array(f).slice(0,Math.min(f.byteLength,r-o)),t),t+=u.head_length(),o+=u.head_length(),r-o<u.name_length()){o=r;break}u.write_name_bytes(s,t,r-o),t+=u.name_length(),o+=u.name_length(),n=u.d_next}return i.setUint32(_,o,!0),0}return 8},fd_renumber(e,t){if(null!=a.fds[e]&&null!=a.fds[t]){let r=a.fds[t].fd_close();return 0!=r?r:(a.fds[t]=a.fds[e],a.fds[e]=void 0,0)}return 8},fd_seek(e,t,r,n){let _=new DataView(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let{ret:i,offset:s}=a.fds[e].fd_seek(t,r);return _.setBigInt64(n,s,!0),i}return 8},fd_sync:e=>null!=a.fds[e]?a.fds[e].fd_sync():8,fd_tell(e,t){let r=new DataView(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let{ret:n,offset:_}=a.fds[e].fd_tell();return r.setBigUint64(t,_,!0),n}return 8},fd_write(e,t,n,_){let i=new DataView(a.inst.exports.memory.buffer),s=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let o=r.read_bytes_array(i,t,n),{ret:l,nwritten:u}=a.fds[e].fd_write(s,o);return i.setUint32(_,u,!0),l}return 8},path_create_directory(e,t,r){let n=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return a.fds[e].path_create_directory(_)}},path_filestat_get(e,t,r,n,_){let i=new DataView(a.inst.exports.memory.buffer),s=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let o=new TextDecoder(\"utf-8\").decode(s.slice(r,r+n)),{ret:l,filestat:u}=a.fds[e].path_filestat_get(t,o);return null!=u&&u.write_bytes(i,_),l}return 8},path_filestat_set_times(e,t,r,n,_,i,s){let o=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let l=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n));return a.fds[e].path_filestat_set_times(t,l,_,i,s)}return 8},path_link(e,t,r,n,_,i,s){let o=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]&&null!=a.fds[_]){let l=new TextDecoder(\"utf-8\").decode(o.slice(r,r+n)),u=new TextDecoder(\"utf-8\").decode(o.slice(i,i+s));return a.fds[_].path_link(e,t,l,u)}return 8},path_open(e,t,r,n,_,i,s,o,l){let u=new DataView(a.inst.exports.memory.buffer),f=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let c=new TextDecoder(\"utf-8\").decode(f.slice(r,r+n)),{ret:d,fd_obj:p}=a.fds[e].path_open(t,c,_,i,s,o);if(0!=d)return d;a.fds.push(p);let E=a.fds.length-1;return u.setUint32(l,E,!0),0}return 8},path_readlink(e,t,r,n,_,i){let s=new DataView(a.inst.exports.memory.buffer),o=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let l=new TextDecoder(\"utf-8\").decode(o.slice(t,t+r)),{ret:u,data:f}=a.fds[e].path_readlink(l);if(null!=f){if(f.length>_)return s.setUint32(i,0,!0),8;o.set(f,n),s.setUint32(i,f.length,!0)}return u}return 8},path_remove_directory(e,t,r){let n=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return a.fds[e].path_remove_directory(_)}return 8},path_rename(e,t,r,n,_,a){throw\"FIXME what is the best abstraction for this?\"},path_symlink(e,t,r,n,_){let i=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[r]){let s=new TextDecoder(\"utf-8\").decode(i.slice(e,e+t)),o=new TextDecoder(\"utf-8\").decode(i.slice(n,n+_));return a.fds[r].path_symlink(s,o)}return 8},path_unlink_file(e,t,r){let n=new Uint8Array(a.inst.exports.memory.buffer);if(null!=a.fds[e]){let _=new TextDecoder(\"utf-8\").decode(n.slice(t,t+r));return a.fds[e].path_unlink_file(_)}return 8},poll_oneoff(e,t,r){throw\"async io not supported\"},proc_exit(e){throw\"exit with exit code \"+e},proc_raise(e){throw\"raised signal \"+e},sched_yield(){},random_get(e,t){let r=new Uint8Array(a.inst.exports.memory.buffer);for(let n=0;n<t;n++)r[e+n]=256*Math.random()|0},sock_recv(e,t,r){throw\"sockets not supported\"},sock_send(e,t,r){throw\"sockets not supported\"},sock_shutdown(e,t){throw\"sockets not supported\"},sock_accept(e,t){throw\"sockets not supported\"}}}}([],[],[new i(new s([])),new i(new s([])),new i(new s([]))]),[4,WebAssembly.compileStreaming(fetch(n))];case 1:return a=e.sent(),[4,WebAssembly.instantiate(a,{wasi_snapshot_preview1:_.wasiImport})];case 2:return o=e.sent(),_.inst=o,o.exports.hs_init(0,0),u(o),[2]}}))}))},window.startDevClient=function(t){return(0,e.mG)(this,void 0,void 0,(function(){var r,n=this;return(0,e.Jh)(this,(function(_){return(r=new WebSocket(t)).onopen=function(e){var t=l.y.encode({tag:l.bs.Start});r.send(t)},r.onmessage=function(t){return(0,e.mG)(n,void 0,void 0,(function(){var n;return(0,e.Jh)(this,(function(_){switch(_.label){case 0:return[4,(a=t.data,new Promise((function(e,t){var r=new FileReader;r.onload=function(){var t=r.result,n=new Uint8Array(t);e(n)},r.onerror=function(e){t(e)},r.readAsArrayBuffer(a)})))];case 1:return n=_.sent(),function(t,r){(0,e.mG)(this,void 0,void 0,(function(){var n,_;return(0,e.Jh)(this,(function(e){switch(t.tag){case l.ur.EvalExpr:return n=l.sN(c,null,r,t.expr),_=l.Xq(n),[2,r({tag:l.bs.Return,0:_})];case l.ur.HotReload:return window.location.reload(),[2];case l.ur.Exit:return[2]}return(0,o.R)(t),[2]}))}))}(l.Cs.decode(n),(function(e){return r.send(l.y.encode(e))})),[2]}var a}))}))},r.onerror=function(e){console.error(\"WebSocket error:\",e)},r.onclose=function(e){console.log(\"WebSocket connection closed:\",e)},[2]}))}))}})()})();"
