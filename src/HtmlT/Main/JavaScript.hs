{-|
Description : Implement interpreter for 'Expr' for JavaScript Backend

TODO: Many conversions between JSVal.JSVal and native JSVal could be
implemented more efficiently. I'm thinking if I can use JavaScript
strings as internal implementation for Data.Text.Text as it was done
in reflex-platform to reduce the overhead. For now current code is
good enough for me, because I consider WebAssembly the main target for
the browser
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
module HtmlT.Main.JavaScript where

import Data.Binary qualified as Binary
import Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Unsafe qualified as BSU
import Data.ByteString.Char8 qualified as Char8
import Data.Text.Internal
import Data.Text qualified as Text
import Data.Word
import Data.Function
import Control.Monad
import System.IO.Unsafe
import Data.Int
import GHC.Exts
import GHC.JS.Prim
import GHC.ForeignPtr
import Data.Text.Array
import GHC.JS.Foreign.Callback
import Unsafe.Coerce

import "this" HtmlT.Base
import "this" HtmlT.Protocol
import "this" HtmlT.RJS
import "this" HtmlT.Protocol.JSNumber
import "this" HtmlT.Protocol.JSVal qualified as JSVal


jsReactorApp :: (StartFlags -> RJS ()) -> IO ()
jsReactorApp jsMain = do
  flags <- mkStartFlags
  let
    loop jsMsg = do
      haskMsg <- handleClientMessage rjsInstance jsMain (BrowserMessage jsMsg)
      evaluateHaskMessage loop haskMsg
  loop (Start flags)

mkStartFlags :: IO StartFlags
mkStartFlags = do
  jvec <- js_mkStartFlags
  protocol <- fmap textFromJsString $ js_indexProp jvec 0
  hostname <- fmap textFromJsString $ js_indexProp jvec 1
  port <- fmap textFromJsString $ js_indexProp jvec 2
  pathname <- fmap textFromJsString $ js_indexProp jvec 3
  search <- fmap textFromJsString $ js_indexProp jvec 4
  hash <- fmap textFromJsString $ js_indexProp jvec 5
  innerWidth <- fmap jsvalToInt64 $ js_indexProp jvec 6
  innerHeight <- fmap jsvalToInt64 $ js_indexProp jvec 7
  return StartFlags
    { initial_url = Location
      { protocol, hostname, port, pathname, search, hash
      }
    , window_inner_size = (innerWidth, innerHeight)
    }

rjsInstance :: RjsInstance
rjsInstance = unsafePerformIO newRjsInstance

evaluateHaskMessage :: (JavaScriptMessage -> IO ()) -> HaskellMessage -> IO ()
evaluateHaskMessage jsMessage = \case
  EvalExpr expr -> do
    result <- evaluateExpr [] jsMessage expr
    jresult <- jsvalToJsval result
    jsMessage (Return jresult)
  Yield expr -> do
    result <- evaluateExpr [] jsMessage expr
    jresult <- jsvalToJsval result
    jsMessage (Return jresult)
  HotReload -> return ()
  Done -> return ()

type Arguments = JSVal

evaluateExpr :: [Arguments] -> (JavaScriptMessage -> IO ()) -> Expr -> IO JSVal
evaluateExpr argScope jsCallback = \case
  NullE ->
    return jsNull

  BooleanE a ->
    return $ if a then js_true else js_false

  NumberE (JSNumber decimalBytes) ->
    return $ js_Number $ bytestringToJsString decimalBytes

  StringE (Text (ByteArray byteArr) offset length) ->
   return $ js_textToJsString byteArr offset length

  ArrayE xs ->
    forM xs (evaluateExpr argScope jsCallback) >>= toJSArray

  ObjectE xs ->
    forM xs evaluatePair >>= toJSArray >>= js_mkObjectFromPairs
    where
      evaluatePair (k, v) = do
        jv <- evaluateExpr argScope jsCallback v
        return $ js_mkPair (textToJsString k) jv

  Dot obj prop -> do
    jobj <- evaluateExpr argScope jsCallback obj
    js_getProp jobj (textToJsString prop)

  AssignProp obj prop val -> do
    jsobj <- evaluateExpr argScope jsCallback obj
    jsval <- evaluateExpr argScope jsCallback val
    js_assignProp jsobj (textToJsString prop) jsval

  Ix val ix -> do
    jsval <- evaluateExpr argScope jsCallback val
    js_indexProp jsval (fromIntegral ix)

  Add a b -> do
    jsa <- evaluateExpr argScope jsCallback a
    jsb <- evaluateExpr argScope jsCallback b
    return $ js_add jsa jsb

  Subtract a b -> do
    jsa <- evaluateExpr argScope jsCallback a
    jsb <- evaluateExpr argScope jsCallback b
    return $ js_substract jsa jsb

  Multiply a b -> do
    jsa <- evaluateExpr argScope jsCallback a
    jsb <- evaluateExpr argScope jsCallback b
    return $ js_multiply jsa jsb

  Divide a b -> do
    jsa <- evaluateExpr argScope jsCallback a
    jsb <- evaluateExpr argScope jsCallback b
    return $ js_divide jsa jsb

  Id name ->
    return $ js_globalId (textToJsString name)

  Lam body -> do
    -- FIXME: syncCallback1' leaks memory, use ForeignPtr?
    evalBodyCb <- syncCallback1' \args ->
      evaluateExpr (args:argScope) jsCallback body
    js_mkLambda evalBodyCb

  Arg lamIx argIx -> do
    let
      loop0 _ [] =
        error $ "Argument index is out of scope: " <> show (Arg lamIx argIx)
      loop0 0 (x:_) = js_indexProp x (fromIntegral argIx)
      loop0 n (x:xs) = loop0 (pred n) xs
    loop0 lamIx argScope

  Apply fun args -> do
    jfun <- evaluateExpr argScope jsCallback fun
    jargs <- mapM (evaluateExpr argScope jsCallback) args >>= toJSArray
    js_apply jfun jargs

  Call fun method args -> do
    jfun <- evaluateExpr argScope jsCallback fun
    jargs <- mapM (evaluateExpr argScope jsCallback) args >>= toJSArray
    js_call jfun (textToJsString method) jargs

  AssignVar (VarId scopeId varId) val -> do
    jval <- evaluateExpr argScope jsCallback val
    js_assignVar (fromIntegral scopeId) (fromIntegral varId) jval
    return jval

  FreeVar (VarId scopeId varId) -> do
    js_freeVar (fromIntegral scopeId) (fromIntegral varId)
    return jsNull

  Var (VarId scopeId varId) ->
    js_readVar (fromIntegral scopeId) (fromIntegral varId)

  FreeScope scopeId -> do
    js_freeScope (fromIntegral scopeId)
    return jsNull

  InsertNode parent child -> do
    jparent <- evaluateExpr argScope jsCallback parent
    jchild <- evaluateExpr argScope jsCallback child
    js_insertNode jparent jchild
    return jsNull

  WithDomBuilder dest builderFn -> do
    jdest <- evaluateExpr argScope jsCallback dest
    jbuilderFn <- evaluateExpr argScope jsCallback builderFn
    jargs <- toJSArray [jdest]
    js_apply jbuilderFn jargs
    return jdest

  CreateElement tagname ->
    js_createElement (textToJsString tagname)

  CreateElementNS ns tagname ->
    js_createElementNs (textToJsString ns) (textToJsString tagname)

  CreateText content ->
    js_createText (textToJsString content)

  ElementProp elm propName propVal -> do
    jelm <- evaluateExpr argScope jsCallback elm
    jval <- evaluateExpr argScope jsCallback propVal
    js_assignProp jelm (textToJsString propName) jval
    return jsNull

  ElementAttr elm attrName attrVal -> do
    jelm <- evaluateExpr argScope jsCallback elm
    js_elementAttr jelm (textToJsString attrName) (textToJsString attrVal)
    return jsNull

  AddEventListener elm eventName listener -> do
    jelm <- evaluateExpr argScope jsCallback elm
    jlistener <- evaluateExpr argScope jsCallback listener
    js_addEventListener jelm (textToJsString eventName) jlistener
    return jsNull

  ToggleClass elm className enabled -> do
    jelm <- evaluateExpr argScope jsCallback elm
    if enabled
      then js_addClass jelm (textToJsString className)
      else js_removeClass jelm (textToJsString className)
    return jsNull

  AssignText txtNode content -> do
    jtxtnode <- evaluateExpr argScope jsCallback txtNode
    js_assignText jtxtnode (textToJsString content)
    return jsNull

  InsertBoundary dest -> do
    jdest <- evaluateExpr argScope jsCallback dest
    js_insertBoundary jdest

  ClearBoundary boundary detach -> do
    jboundary <- evaluateExpr argScope jsCallback boundary
    js_clearBoundary jboundary detach
    return jsNull

  RevSeq seq -> do
    let
      loop [] = return jsNull
      loop [x] = evaluateExpr argScope jsCallback x
      loop (x:xs) = loop xs >> evaluateExpr argScope jsCallback x
    loop seq

  Eval rawjs ->
    js_eval (textToJsString rawjs)

  TriggerEvent callbackId arg -> do
    jarg <- jsvalToJsval =<< evaluateExpr argScope jsCallback arg
    jsCallback $ TriggerEventMsg jarg callbackId
    return jsNull

  TriggerAnimation callbackId arg -> do
    jarg <- jsvalToJsval =<< evaluateExpr argScope jsCallback arg
    jsCallback $ TriggerAnimationMsg jarg callbackId
    return jsNull

  TriggerCallback callbackId arg -> do
    jarg <- jsvalToJsval =<< evaluateExpr argScope jsCallback arg
    jsCallback $ TriggerCallbackMsg jarg callbackId
    return jsNull

  UncaughtException msg -> do
    js_uncaughtException (textToJsString msg)
    return jsNull

jsvalToInt64 :: JSVal -> Int64
jsvalToInt64 = fromIntegral . js_jsvalToInt

jsvalToJsval :: JSVal -> IO JSVal.JSVal
jsvalToJsval jsval = do
  jtag <- jsTypeTag jsval
  case jtag of
    (0, _jval) ->
      return JSVal.Null
    (1, jval) -> return $ unsafeCoerce $
      js_bool
        (unsafeCoerce (JSVal.Bool False))
        (unsafeCoerce (JSVal.Bool True)) jval
    (2, jval) ->
      return (JSVal.Number (jsNumberFromInt64 (jsvalToInt64 jval)))
    (3, jval) ->
      return (JSVal.String (textFromJsString jval))
    (4, jval) ->
      fromJSArray jval >>= fmap JSVal.Array . mapM jsvalToJsval
    (5, jval) ->
      fromJSArray jval >>= fmap JSVal.Object . mapM \jtuple -> do
        key <- textFromJsString <$> js_indexProp jtuple 0
        val <- jsvalToJsval =<< js_indexProp jtuple 1
        return (key, val)
    _ ->
      error "js_typeOf returned invalid type tag number"
  where
    jsTypeTag :: JSVal -> IO (Int, JSVal)
    jsTypeTag jsval = do
      let jtuple = js_typeOf jsval
      tag <- js_indexProp jtuple 0
      val <- js_indexProp jtuple 1
      return (js_jsvalToInt tag, val)

textFromJsString :: JSVal -> Text
textFromJsString jsString = unsafePerformIO do
  barr <- js_stringToByteArray jsString
  return $ Text barr 0 (js_strlen jsString)

textToJsString :: Text -> JSVal
textToJsString (Text (ByteArray byteArr) off len) =
  js_textToJsString byteArr off len

bytestringToJsString :: ByteString -> JSVal
bytestringToJsString (BS (ForeignPtr addr _) len) =
  js_bytestringToJsString addr len

foreign import javascript unsafe "(() => { return true; })"
  js_true :: JSVal
foreign import javascript unsafe "(() => { return false; })"
  js_false :: JSVal
foreign import javascript unsafe "((obj, prop) => { return obj[prop]; })"
  js_getProp :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "((obj, prop, val) => { obj[prop] = val; })"
  js_assignProp :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "((obj, index) => { return obj[index]; })"
  js_indexProp :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "((a, b) => { return a + b; })"
  js_add :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "((a, b) => { return a - b; })"
  js_substract :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "((a, b) => { return a * b; })"
  js_multiply :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "((a, b) => { return a / b; })"
  js_divide :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "((name) => { return window[name]; })"
  js_globalId :: JSVal -> JSVal
foreign import javascript unsafe "((fun, args) => { return fun.apply(undefined, args); })"
  js_apply :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe
  "((obj, method, args) => { var fn = obj[method]; return fn.apply(obj, args); })"
  js_call :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe
  "((scopeId, varId, val) => {\
    if (!window.varStorage) window.varStorage = new Map();\
    if (window.varStorage.has(scopeId)) {\
      var scopeMap = window.varStorage.get(scopeId);\
      scopeMap.set(varId, val);\
    } else {\
      var scopeMap = new Map();\
      scopeMap.set(varId, val);\
      window.varStorage.set(scopeId, scopeMap);\
    }\
  })"
  js_assignVar :: Int -> Int -> JSVal -> IO ()
foreign import javascript unsafe
  "((scopeId, varId, val) => {\
    var scopeStorage = window.varStorage.get(scopeId);\
    if (!scopeStorage) return;\
    scopeStorage.delete(varId);\
    if (scopeStorage.size == 0) {\
      window.varStorage.delete(scopeId);\
    }\
  })"
  js_freeVar :: Int -> Int -> IO ()
foreign import javascript unsafe
  "(scopeId => { window.varStorage.delete(scopeId); })"
  js_freeScope :: Int -> IO ()
foreign import javascript unsafe
  "((scopeId, varId) => { return window.varStorage.get(scopeId).get(varId); })"
  js_readVar :: Int -> Int -> IO JSVal
foreign import javascript unsafe "(rawjs => { return eval(rawjs); })"
  js_eval :: JSVal -> IO JSVal
foreign import javascript unsafe
  "(callback => { return function() { return callback(arguments); } })"
  js_mkLambda :: Callback (JSVal -> IO JSVal) -> IO JSVal
foreign import javascript unsafe
  "((dest, childNode) => {\
    if (dest instanceof Comment) {\
      dest.parentElement.insertBefore(childNode, dest);\
    } else {\
      dest.appendChild(childNode);\
    }\
  })"
  js_insertNode :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "(tagName => { return document.createElement(tagName); })"
  js_createElement :: JSVal -> IO JSVal
foreign import javascript unsafe
  "((ns, tagName) => { return document.createElementNS(ns, tagName); })"
  js_createElementNs :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe
  "(content => { return document.createTextNode(content); })"
  js_createText :: JSVal -> IO JSVal
foreign import javascript unsafe
  "((dest, propName, propVal) => {\
    if (dest instanceof Comment) {\
      dest.parentElement[propName] = propVal;\
    } else {\
      dest[propName] = propVal;\
    }\
  })"
  js_elementProp :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((dest, attrName, attrVal) => {\
    if (dest instanceof Comment) {\
      dest.parentElement.setAttribute(attrName, attrVal);\
    } else {\
      dest.setAttribute(attrName, attrVal);\
    }\
  })"
  js_elementAttr :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((dest, eventName, listener) => {\
    if (dest instanceof Comment) {\
      dest.parentElement.addEventListener(eventName, listener);\
    } else {\
      dest.addEventListener(eventName, listener);\
    }\
  })"
  js_addEventListener :: JSVal -> JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((dest, className) => {\
    if (dest instanceof Comment) {\
      dest.parentElement.classList.add(className);\
    } else {\
      dest.classList.add(className);\
    }\
  })"
  js_addClass :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((dest, className) => {\
    if (dest instanceof Comment) {\
      dest.parentElement.classList.remove(className);\
    } else {\
      dest.classList.remove(className);\
    }\
  })"
  js_removeClass :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((node, content) => { node.textContent = content; })"
  js_assignText :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe
  "((dest) => {\
    function insertIntoBuilder(childNode) {\
      if (dest instanceof Comment) {\
        dest.parentElement.insertBefore(childNode, dest);\
      } else {\
        dest.appendChild(childNode);\
      }\
    }\
    var begin = document.createComment('ContentBoundary {{');\
    var end = document.createComment('}}');\
    insertIntoBuilder(begin);\
    insertIntoBuilder(end);\
    return end;\
  })"
  js_insertBoundary :: JSVal -> IO JSVal
foreign import javascript unsafe
  "((boundaryNode, detach) => {\
    function isOpeningBoundary(node) {\
      if (node instanceof Comment && node.textContent == 'ContentBoundary {{') {\
        return true;\
      }\
      return false;\
    }\
    function isClosingBoundary(node) {\
      if (node instanceof Comment && node.textContent == '}}') {\
        return true;\
      }\
      return false;\
    }\
    var end = boundaryNode;\
    var nestedCounter = 0;\
    for (;;){\
      if (!end.previousSibling ||\
        (nestedCounter == 0 && isOpeningBoundary(end.previousSibling))\
        ) break;\
      if (isClosingBoundary(end.previousSibling)) nestedCounter++;\
      else if (isOpeningBoundary(end.previousSibling)) nestedCounter--;\
      end.previousSibling.parentNode.removeChild(end.previousSibling);\
    }\
    if (detach) {\
      end.previousSibling.parentNode.removeChild(end.previousSibling);\
      end.parentNode.removeChild(end);\
    }\
  })"
  js_clearBoundary :: JSVal -> Bool -> IO ()
foreign import javascript unsafe "(u8array => { throw new Error('js_uncaughtException'); })"
  js_uncaughtException :: JSVal -> IO ()
foreign import javascript unsafe
  "(() => {\
    return [\
      location.protocol,\
      location.hostname,\
      location.port,\
      location.pathname,\
      location.search,\
      location.hash,\
      window.innerWidth,\
      window.innerHeight\
    ];\
  })"
  js_mkStartFlags :: IO JSVal
foreign import javascript unsafe "(num => { return num; })"
  js_jsvalToInt :: JSVal -> Int
foreign import javascript unsafe
  "(str => {\
    var byteArray = h$newByteArray(str.length);\
    for (var i = 0; i < str.length; i++) {\
      byteArray.u8[i] = str.charCodeAt(i);\
    }\
    return byteArray;\
  })"
  js_stringToByteArray :: JSVal -> IO Array
foreign import javascript unsafe
  "((byteArr, off, len) => {\
    return (new TextDecoder('utf-8')).decode(byteArr.u8.subarray(off, off + len));\
  })"
  js_textToJsString :: ByteArray# -> Int -> Int -> JSVal
foreign import javascript unsafe
  "((addr, _, len) => {\
    return (new TextDecoder('utf-8')).decode(addr.u8.subarray(0, len));\
  })"
  js_bytestringToJsString :: Addr# -> Int -> JSVal
foreign import javascript unsafe
  "(val => {\
    if (val === null || val === undefined) { \
      return [0, null];\
    }\
    if (typeof(val) === 'boolean') { \
      return [1, val];\
    }\
    if (typeof(val) === 'number') { \
      return [2, val];\
    }\
    if (typeof(val) === 'string') { \
      return [3, val];\
    }\
    if (Array.isArray(val)) { \
      return [4, val];\
    }\
    return [5, Object.entries(val)];\
  })"
  js_typeOf :: JSVal -> JSVal
foreign import javascript unsafe
  "((ifFalse, ifTrue, cond) => {\
    return cond ? ifTrue : ifFalse;\
  })"
  js_bool :: Any -> Any -> JSVal -> Any
foreign import javascript unsafe "(str => { return str.length; })"
  js_strlen :: JSVal -> Int
foreign import javascript unsafe "((a, b) => [a, b])"
  js_mkPair :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe
  "(pairs => pairs.reduce((obj, pair) => {\
    obj[pair[0]] = pair[1];\
    return obj;\
  }, {}))"
  js_mkObjectFromPairs :: JSVal -> IO JSVal
foreign import javascript unsafe "(str => { console.log(str); })"
  js_consoleLog :: JSVal -> IO ()
foreign import javascript unsafe "(str => { return Number(str); })"
  js_Number :: JSVal -> JSVal
