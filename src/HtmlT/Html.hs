module HtmlT.Html where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Map qualified as Map
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List qualified as List

import "this" HtmlT.Event
import "this" HtmlT.Protocol
import "this" HtmlT.RJS
import "this" HtmlT.Protocol.JSVal


newtype Html a = Html { unHtml :: RJS a }

data HtmlState = HtmlState
  { element_var :: Maybe VarId
  }

deriving newtype instance Functor Html
deriving newtype instance Applicative Html
deriving newtype instance Monad Html
deriving newtype instance MonadReader ReactiveScope Html
deriving newtype instance MonadState RjsState Html
deriving newtype instance MonadIO Html
deriving newtype instance MonadFix Html
deriving newtype instance MonadReactive Html

instance MonadRJS Html where liftRJS = Html

instance a ~ () => IsString (Html a) where
  fromString = text . Text.pack

el :: Text -> Html a -> Html a
el tagName (Html content) =
  Html $ withDomBuilder (CreateElement tagName) content

elns :: Text -> Text -> Html a -> Html a
elns ns tagName (Html content) =
  Html $ withDomBuilder (CreateElementNS ns tagName) content

prop :: ToJSVal v => Text -> v -> Html ()
prop propName propVal = modify \s ->
  let
    expr = ElementProp (Arg 0 0) propName (jsvalToExpr (toJSVal propVal))
  in
    s {evaluation_queue = expr : s.evaluation_queue }

attr :: Text -> Text -> Html ()
attr attrName attrVal = modify \s ->
  let
    expr = ElementAttr (Arg 0 0) attrName attrVal
  in
    s {evaluation_queue = expr : s.evaluation_queue}

dynProp :: (ToJSVal v, Eq v) => Text -> Dynamic v -> Html ()
dynProp propName (holdUniqDyn -> valueDyn) = Html do
  rscope <- ask
  initialVal <- readDyn valueDyn
  currentNodeVar <- newVar
  let
    initProp = ElementProp (Arg 0 0) propName (jsvalToExpr (toJSVal initialVal))
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {evaluation_queue = saveNode : initProp : s.evaluation_queue }
  valueDyn.updates.subscribe $ enqueueIfAlive rscope .
    ElementProp (Var currentNodeVar) propName . jsvalToExpr . toJSVal

dynAttr :: Text -> Dynamic Text -> Html ()
dynAttr attrName (holdUniqDyn -> valueDyn) = Html do
  rscope <- ask
  initialVal <- readDyn valueDyn
  currentNodeVar <- newVar
  let
    initProp = ElementAttr (Arg 0 0) attrName initialVal
    saveNode = AssignVar currentNodeVar (Arg 0 0)
  modify \s -> s {evaluation_queue = saveNode : initProp : s.evaluation_queue }
  valueDyn.updates.subscribe
    $ enqueueIfAlive rscope
    . ElementAttr (Var currentNodeVar) attrName

toggleClass :: Text -> Dynamic Bool -> Html ()
toggleClass className enableDyn = Html do
  reactiveScope <- ask
  currentNodeVar <- newVar
  initVal <- liftIO enableDyn.sample
  let
    enableUniqDyn = holdUniqDyn enableDyn
    initCmd False queue = queue
    initCmd True queue = InsertClassList (Arg 0 0) [className] : queue
    updateCmd False queue = RemoveClassList (Var currentNodeVar) [className] : queue
    updateCmd True queue = InsertClassList (Var currentNodeVar) [className] : queue
    saveNode = AssignVar currentNodeVar (Arg 0 0)
    modQueueIfAlive f = modify \s -> s
      { evaluation_queue =
        if Map.member reactiveScope s.finalizers
          then f s.evaluation_queue else s.evaluation_queue
      }
  modify \s -> s {evaluation_queue = saveNode : initCmd initVal s.evaluation_queue}
  enableUniqDyn.updates.subscribe $ modQueueIfAlive . updateCmd

dynClassList :: Dynamic [Text] -> Html ()
dynClassList dynList = Html do
  reactiveScope <- ask
  currentNodeVar <- newVar
  initVal <- liftIO dynList.sample
  let
    compareList as bs =
      (diffList as bs, diffList bs as)
    diffList as bs = List.foldl'
      (\xs k -> if List.elem k as then xs else k:xs) [] bs
    f newList (_, _, oldList) =
      let
        (added, removed) = compareList oldList newList
      in
        Just (added, removed, newList)
  dynListDiff <- foldDynMaybe f ([], [], []) dynList
  let
    initCmd = InsertClassList (Arg 0 0) initVal
    updateCmd ([], [], _) queue = queue
    updateCmd (added, [], _) queue = InsertClassList (Var currentNodeVar) added : queue
    updateCmd ([], removed, _) queue = RemoveClassList (Var currentNodeVar) removed : queue
    updateCmd (added, removed, _) queue = RemoveClassList (Var currentNodeVar) removed : InsertClassList (Var currentNodeVar) added : queue
    saveNode = AssignVar currentNodeVar (Arg 0 0)
    modQueueIfAlive f = modify \s -> s
      { evaluation_queue =
        if Map.member reactiveScope s.finalizers
          then f s.evaluation_queue else s.evaluation_queue
      }
  modify \s -> s {evaluation_queue = saveNode : initCmd : s.evaluation_queue}
  dynListDiff.updates.subscribe $ modQueueIfAlive . updateCmd

text :: Text -> Html ()
text contents = do
  let expr = InsertNode (Arg 0 0) (CreateText contents)
  modify \s -> s {evaluation_queue = expr : s.evaluation_queue}

dynText :: Dynamic Text -> Html ()
dynText (holdUniqDyn -> dynContent) = Html do
  rscope <- ask
  initialContent <- readDyn dynContent
  textNodeVar <- newVar
  let
    insertText = InsertNode (Arg 0 0)
      (AssignVar textNodeVar (CreateText initialContent))
  modify \s -> s {evaluation_queue = insertText : s.evaluation_queue }
  dynContent.updates.subscribe $
    enqueueIfAlive rscope . AssignText (Var textNodeVar)

dyn :: Dynamic (Html ()) -> Html ()
dyn d = Html do
  boundary <- insertBoundary
  initialScope <- newScope
  reactiveScopeRef <- liftIO $ newIORef initialScope
  initialVal <- readDyn d
  let
    update html = Html do
      clearBoundary boundary
      html.unHtml
  local (const initialScope) $ withDomBuilder (Var boundary) initialVal.unHtml
  d.updates.subscribe \newVal -> do
    newReactiveScope <- newScope
    oldReactiveScope <- liftIO $ atomicModifyIORef reactiveScopeRef (newReactiveScope,)
    freeScope oldReactiveScope
    local (const newReactiveScope) . attachHtml (Var boundary) $ update newVal

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
simpleList listDyn h = Html do
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  boundary <- insertBoundary
  let
    setup :: Int -> [a] -> [ElemEnv a] -> RJS [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        newElem <- newElemEnv x
        withDomBuilder (Var newElem.elem_boundary)
          $ local (const newElem.reactive_scope)
          $ (.unHtml)
          $ h idx newElem.elem_state_ref
        fmap (newElem:) $ setup (idx + 1) xs []
      -- New list is shorter, delete the elements that no longer
      -- present in the new list
      (r:rs, []) -> do
        finalizeElems True (r:rs)
        return []
      -- Update existing elements along the way
      (r:rs, y:ys) -> do
        writeRef r.elem_state_ref y
        fmap (r:) $ setup (idx + 1) ys rs
    newElemEnv :: a -> RJS (ElemEnv a)
    newElemEnv a = do
      reactive_scope <- newScope
      (local (const reactive_scope)) do
        elem_state_ref <- newRef a
        elem_boundary <- insertBoundary
        return ElemEnv {reactive_scope, elem_state_ref, elem_boundary}
    finalizeElems :: Bool -> [ElemEnv a] -> RJS ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ destroyBoundary ee.elem_boundary
      freeScope ee.reactive_scope
    updateList new = Html do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
  initialVal <- readDyn listDyn
  withDomBuilder (Var boundary) . (.unHtml) $ updateList initialVal
  listDyn.updates.subscribe $ attachHtml (Var boundary) . updateList

insertBoundary :: RJS VarId
insertBoundary = do
  boundary <- newVar
  modify \s -> s {evaluation_queue = AssignVar boundary (InsertBoundary (Arg 0 0)) : s.evaluation_queue}
  return boundary

clearBoundary :: VarId -> RJS ()
clearBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) False)

destroyBoundary :: VarId -> RJS ()
destroyBoundary boundary = enqueueExpr (ClearBoundary (Var boundary) True)

attachHtml :: Expr -> Html a -> RJS a
attachHtml builder contents = do
  saveQueue <- state \s ->
    (s.evaluation_queue, s {evaluation_queue = []})
  result <- contents.unHtml
  modify \s ->
    let
      attachExpr = WithDomBuilder builder (Lam (RevSeq s.evaluation_queue))
    in
      s {evaluation_queue = attachExpr : saveQueue}
  return result

withDomBuilder :: Expr -> RJS a -> RJS a
withDomBuilder builder content = do
  prevQueue <- state \s -> (s.evaluation_queue, s {evaluation_queue = []})
  result <- content
  let
    mkExpr mkContent = InsertNode (Arg 0 0)
      (WithDomBuilder builder (Lam (RevSeq mkContent)))
  modify \s -> s {evaluation_queue = mkExpr s.evaluation_queue : prevQueue}
  return result

attachToBody :: Html a -> RJS a
attachToBody = attachHtml (Id "document" `Dot` "body")

blank :: Applicative m => m ()
blank = pure ()
{-# INLINE blank #-}
