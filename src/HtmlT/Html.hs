module HtmlT.Html where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Fix
import Data.IORef
import Data.String
import GHC.Generics
import Data.Text (Text)
import Data.Text qualified as Text

import "this" HtmlT.Event
import "this" HtmlT.Protocol
import "this" HtmlT.RJS
import "this" HtmlT.Protocol.JSVal


newtype HtmlT m a = HtmlT {unHtmlT :: StateT HtmlState m a}

execHtmlT :: Monad m => HtmlState -> HtmlT m a -> m (a, HtmlState)
execHtmlT s = flip runStateT s . unHtmlT

type Html = HtmlT RJS

data HtmlState = HtmlState
  { rev_queue :: [Expr]
  , save_current_node :: Maybe VarId
  } deriving (Generic)

el :: Text -> Html a -> Html a
el tagName = withBuilder (CreateElement tagName)

elns :: Text -> Text -> Html a -> Html a
elns ns tagName = withBuilder (CreateElementNS ns tagName)

prop :: ToJSVal v => Text -> v -> Html ()
prop propName propVal = modify \s ->
  let
    expr = ElementProp (Arg 0 0) propName (jsvalToExpr (toJSVal propVal))
  in
    s {rev_queue = expr : s.rev_queue }

attr :: Text -> Text -> Html ()
attr attrName attrVal = modify \s ->
  let
    expr = ElementAttr (Arg 0 0) attrName attrVal
  in
    s {rev_queue = expr : s.rev_queue}

dynProp :: (ToJSVal v, Eq v) => Text -> Dynamic v -> Html ()
dynProp propName (holdUniqDyn -> valueDyn) = do
  rscope <- lift $ gets (.reactive_scope)
  initialVal <- readDyn valueDyn
  currentNodeVar <- lift newVar
  let
    initProp = ElementProp (Arg 0 0) propName (jsvalToExpr (toJSVal initialVal))
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {rev_queue = saveNode : initProp : s.rev_queue }
  lift $ subscribe (updates valueDyn)
    $ enqueueIfAlive rscope
    . ElementProp (Var currentNodeVar) propName . jsvalToExpr . toJSVal

dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr attrName (holdUniqDyn -> valueDyn) = do
  rscope <- lift $ gets (.reactive_scope)
  initialVal <- readDyn valueDyn
  currentNodeVar <- lift newVar
  let
    initProp = ElementAttr (Arg 0 0) attrName initialVal
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {rev_queue = saveNode : initProp : s.rev_queue }
  lift $ subscribe (updates valueDyn)
    $ enqueueIfAlive rscope
    . ElementAttr (Var currentNodeVar) attrName

toggleClass :: Text -> Dynamic Bool -> Html ()
toggleClass className (holdUniqDyn -> enableDyn) = do
  rscope <- lift $ gets (.reactive_scope)
  initialVal <- readDyn enableDyn
  currentNodeVar <- lift newVar
  let
    initClass = ToggleClass (Arg 0 0) className initialVal
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {rev_queue = saveNode : initClass : s.rev_queue }
  lift $ subscribe (updates enableDyn) $
    enqueueIfAlive rscope . ToggleClass (Var currentNodeVar) className

text :: Text -> Html ()
text contents = do
  let expr = InsertNode (Arg 0 0) (CreateText contents)
  modify \s -> s {rev_queue = expr : s.rev_queue}

dynText :: Dynamic Text -> Html ()
dynText (holdUniqDyn -> dynContent) = do
  rscope <- lift $ gets (.reactive_scope)
  initialContent <- readDyn dynContent
  textNodeVar <- lift newVar
  let
    insertText = InsertNode (Arg 0 0)
      (AssignVar textNodeVar (CreateText initialContent))
  modify \s -> s {rev_queue = insertText : s.rev_queue }
  lift $ subscribe (updates dynContent) $
    enqueueIfAlive rscope . AssignText (Var textNodeVar)

dyn :: Dynamic (Html ()) -> Html ()
dyn d = do
  boundary <- insertBoundary
  initialScope <- lift newScope
  reactiveScopeRef <- liftIO $ newIORef initialScope
  initialVal <- readDyn d
  let
    update html = do
      lift $ clearBoundary boundary
      html
  monohoist (localScope initialScope) $ withBuilder (Var boundary) initialVal
  lift $ subscribe (updates d) \newVal -> do
    newReactiveScope <- newScope
    oldReactiveScope <- liftIO $ atomicModifyIORef reactiveScopeRef (newReactiveScope,)
    freeScope oldReactiveScope
    localScope newReactiveScope . attachHtml (Var boundary) $ update newVal

-- | Auxilliary datatype that helps to implement 'simpleList'
data ElemEnv a = ElemEnv
  { elem_boundary :: VarId
  , elem_state_ref :: DynRef a
  , reactive_scope :: ReactiveScope
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
        withBuilder (Var newElem.elem_boundary)
          $ monohoist (localScope newElem.reactive_scope)
          $ h idx newElem.elem_state_ref
        fmap (newElem:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        lift $ writeRef r.elem_state_ref y
        fmap (r:) $ setup (idx + 1) ys rs
    newElemEnv :: a -> Html (ElemEnv a)
    newElemEnv a = do
      reactive_scope <- lift newScope
      monohoist (localScope reactive_scope) do
        elem_state_ref <- lift $ newRef a
        elem_boundary <- insertBoundary
        return ElemEnv {reactive_scope, elem_state_ref, elem_boundary}
    finalizeElems :: Bool -> [ElemEnv a] -> Html ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ lift $ destroyBoundary ee.elem_boundary
      lift $ freeScope ee.reactive_scope
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readDyn listDyn
  withBuilder (Var boundary) $ updateList initialVal
  lift $ subscribe (updates listDyn) $ attachHtml (Var boundary) . updateList

monohoist :: forall a. (forall a. RJS a -> RJS a) -> Html a -> Html a
monohoist f (HtmlT (StateT g)) = HtmlT $ StateT \s -> f (g s)

insertBoundary :: Html VarId
insertBoundary = do
  boundary <- lift newVar
  modify \s -> s {rev_queue = AssignVar boundary (InsertBoundary (Arg 0 0)) : s.rev_queue}
  return boundary

clearBoundary :: VarId -> RJS ()
clearBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) False)

destroyBoundary :: VarId -> RJS ()
destroyBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) True)

attachHtml :: Expr -> Html a -> RJS a
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

attachToBody :: Html a -> RJS a
attachToBody = attachHtml (Id "document" `Dot` "body")

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}

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
  fromString = text . Text.pack
