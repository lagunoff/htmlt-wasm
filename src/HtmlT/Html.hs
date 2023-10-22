module HtmlT.Html where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Fix
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.String
import GHC.Generics

import "this" HtmlT.Base
import "this" HtmlT.Event
import "this" HtmlT.JSM
import "this" HtmlT.Marshal
import "this" HtmlT.Protocol
import "this" HtmlT.Protocol.Utf8 (Utf8(..))

newtype HtmlT m a = HtmlT {unHtmlT :: StateT HtmlState m a}

execHtmlT :: Monad m => HtmlState -> HtmlT m a -> m (a, HtmlState)
execHtmlT s = flip runStateT s . unHtmlT

type Html = HtmlT JSM

data HtmlState = HtmlState
  { rev_queue :: [Expr]
  , save_current_node :: Maybe VarId
  } deriving (Generic)

el :: Utf8 -> Html a -> Html a
el tagName = withBuilder (CreateElement tagName)

prop :: ToJSVal v => Utf8 -> v -> Html ()
prop propName propVal = modify \s ->
  let
    expr = ElementProp (Arg 0 0) propName (fromJValue (toJSVal propVal))
  in
    s {rev_queue = expr : s.rev_queue }

attr :: Utf8 -> Utf8 -> Html ()
attr attrName attrVal = modify \s ->
  let
    expr = ElementAttr (Arg 0 0) attrName attrVal
  in
    s {rev_queue = expr : s.rev_queue}

dynProp :: (ToJSVal v, Eq v) => Utf8 -> Dynamic v -> Html ()
dynProp propName (holdUniqDyn -> valueDyn) = do
  initialVal <- readDyn valueDyn
  currentNodeVar <- lift newVar
  let
    initProp = ElementProp (Arg 0 0) propName (fromJValue (toJSVal initialVal))
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {rev_queue = saveNode : initProp : s.rev_queue }
  lift $ subscribe (updates valueDyn)
    $ queueIfAlive currentNodeVar
    . ElementProp (Var currentNodeVar) propName . fromJValue . toJSVal

toggleClass :: Utf8 -> Dynamic Bool -> Html ()
toggleClass className (holdUniqDyn -> enableDyn) = do
  initialVal <- readDyn enableDyn
  currentNodeVar <- lift newVar
  let
    initClass = ToggleClass (Arg 0 0) className initialVal
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {rev_queue = saveNode : initClass : s.rev_queue }
  lift $ subscribe (updates enableDyn) $
    queueIfAlive currentNodeVar . ToggleClass (Var currentNodeVar) className

text :: Utf8 -> Html ()
text contents = do
  let expr = InsertNode (Arg 0 0) (CreateText contents)
  modify \s -> s {rev_queue = expr : s.rev_queue}

dynText :: Dynamic Utf8 -> Html ()
dynText (holdUniqDyn -> dynContent) = do
  initialContent <- readDyn dynContent
  textNodeVar <- lift newVar
  let
    insertText = InsertNode (Arg 0 0)
      (AssignVar textNodeVar (CreateText initialContent))
  modify \s -> s {rev_queue = insertText : s.rev_queue }
  lift $ subscribe (updates dynContent) $
    queueIfAlive textNodeVar . AssignText (Var textNodeVar)

dyn :: Dynamic (Html ()) -> Html ()
dyn d = do
  boundary <- insertBoundary
  finalizerNs <- lift newNamespace
  let
    update html = do
      lift $ clearBoundary boundary
      lift $ finalizeNamespace finalizerNs
      html
    applyFinalizer e = e {finalizer_ns = finalizerNs}
  initialVal <- readDyn d
  monohoist (local applyFinalizer) $ withBuilder (Var boundary) initialVal
  lift $ subscribe (updates d) $
    local applyFinalizer . attachHtml (Var boundary) . update

-- | Auxilliary datatype that helps to implement 'simpleList'
data ElemEnv a = ElemEnv
  { ee_boundary :: VarId
  , ee_dyn_ref :: DynRef a
  , ee_namespace :: FinalizerNs
  }

simpleList
  :: forall a. Dynamic [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> Html ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Html ()
simpleList listDyn h = do
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  boundary <- insertBoundary
  let
    setup :: Int -> [a] -> [ElemEnv a] -> Html [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        newElem <- newElemEnv x
        let wasmEnv = JSMEnv newElem.ee_namespace
        withBuilder (Var newElem.ee_boundary) $
          monohoist (local (const wasmEnv)) $ h idx newElem.ee_dyn_ref
        fmap (newElem:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        lift $ writeRef r.ee_dyn_ref y
        fmap (r:) $ setup (idx + 1) ys rs
    newElemEnv :: a -> Html (ElemEnv a)
    newElemEnv a = do
      ee_namespace <- lift newNamespace
      monohoist (local (\e -> e {finalizer_ns = ee_namespace})) do
        ee_dyn_ref <- lift $ newRef a
        ee_boundary <- insertBoundary
        return ElemEnv {..}
    finalizeElems :: Bool -> [ElemEnv a] -> Html ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ lift $ destroyBoundary ee.ee_boundary
      lift $ finalizeNamespace ee.ee_namespace
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readDyn listDyn
  withBuilder (Var boundary) $ updateList initialVal
  lift $ subscribe (updates listDyn) $ attachHtml (Var boundary) . updateList

monohoist :: forall a. (forall a. JSM a -> JSM a) -> Html a -> Html a
monohoist f (HtmlT (StateT g)) = HtmlT $ StateT \s -> f (g s)

insertBoundary :: Html VarId
insertBoundary = do
  boundary <- lift newVar
  modify \s -> s {rev_queue = AssignVar boundary (InsertBoundary (Arg 0 0)) : s.rev_queue}
  return boundary

clearBoundary :: VarId -> JSM ()
clearBoundary boundary = queueExp (ClearBoundary (Var boundary) False)

destroyBoundary :: VarId -> JSM ()
destroyBoundary boundary = queueExp (ClearBoundary (Var boundary) True)

attachHtml :: Expr -> Html a -> JSM a
attachHtml builder html = do
  (result, newState) <- execHtmlT (HtmlState [] Nothing) html
  let newExpr = WithBuilder builder (Lam (RevSeq newState.rev_queue))
  modify \s -> s {evaluation_queue = newExpr : s.evaluation_queue}
  return result

withBuilder :: Expr -> Html a -> Html a
withBuilder builder content = do
  prevQueue <- state \s -> (s.rev_queue, s {rev_queue = []})
  result <- content
  let
    mkExpr mkContent = InsertNode (Arg 0 0)
      (WithBuilder builder (Lam (RevSeq mkContent)))
  modify \s -> s {rev_queue = mkExpr s.rev_queue : prevQueue}
  return result

attachToBody :: Html a -> JSM a
attachToBody = attachHtml (Id "document" `Dot` "body")

deriving newtype instance Functor m => Functor (HtmlT m)
deriving newtype instance Monad m => Applicative (HtmlT m)
deriving newtype instance Monad m => Monad (HtmlT m)
deriving newtype instance Monad m => MonadState HtmlState (HtmlT m)
deriving newtype instance MonadReader r m => MonadReader r (HtmlT m)
deriving newtype instance MonadWriter w m => MonadWriter w (HtmlT m)
deriving newtype instance MonadIO m => MonadIO (HtmlT m)
deriving newtype instance MonadTrans HtmlT
deriving newtype instance MonadFix m => MonadFix (HtmlT m)

instance a ~ () => IsString (Html a) where
  fromString = text . Utf8 . Char8.pack
