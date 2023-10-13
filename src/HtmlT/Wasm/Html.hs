{-# OPTIONS_GHC -Wno-orphans #-}
module HtmlT.Wasm.Html where

import Data.ByteString.Char8 qualified as Char8
import Control.Monad.IO.Class
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.String
import GHC.Generics

import "this" HtmlT.Wasm.Types
import "this" HtmlT.Wasm.Base
import "this" HtmlT.Wasm.Protocol
import "this" HtmlT.Wasm.Marshal
import "this" HtmlT.Wasm.Event

newtype HtmlT m a = HtmlT {unHtmlT :: ReaderT HtmlEnv m a}

deriving newtype instance Functor m => Functor (HtmlT m)
deriving newtype instance Applicative m => Applicative (HtmlT m)
deriving newtype instance Monad m => Monad (HtmlT m)
deriving newtype instance MonadState s m => MonadState s (HtmlT m)
deriving newtype instance Monad m => MonadReader HtmlEnv (HtmlT m)
deriving newtype instance MonadWriter w m => MonadWriter w (HtmlT m)
deriving newtype instance MonadIO m => MonadIO (HtmlT m)
deriving newtype instance MonadTrans HtmlT

instance a ~ () => IsString (Html a) where
  fromString = text . Utf8 . Char8.pack

execHtmlT :: HtmlEnv -> HtmlT m a -> m a
execHtmlT e = (`runReaderT` e) . unHtmlT

type Html = HtmlT WA

data HtmlEnv = HtmlEnv
  { dom_builder_id :: DomBuilder
  , save_current_element :: VarId
  } deriving (Generic)

el :: Utf8 -> Html a -> Html a
el tagName child = do
  domBuilderId <- asks (.dom_builder_id)
  save_current_element <- lift $ newVar
  lift $ queueExp (ElPush domBuilderId tagName)
  result <- local (\e -> e {save_current_element}) child
  lift $ queueExp (ElPop domBuilderId)
  return result

prop :: ToJSVal v => Utf8 -> v -> Html ()
prop propName propVal = do
  domBuilderId <- asks (.dom_builder_id)
  lift $ queueExp (ElProp domBuilderId propName (fromJValue (toJSVal propVal)))

dynProp :: (ToJSVal v, Eq v) => Utf8 -> Dynamic v -> Html ()
dynProp propName (holdUniqDyn -> valueDyn) = do
  e <- ask
  initialVal <- readDyn valueDyn
  lift $ queueExp (AssignVar e.save_current_element (Var (unDomBuilder e.dom_builder_id)))
  lift $ queueExp (ElProp e.dom_builder_id propName (fromJValue (toJSVal initialVal)))
  lift $ subscribe (updates valueDyn)
    $ queueIfAlive e.save_current_element
    . ElProp (DomBuilder e.save_current_element) propName . fromJValue . toJSVal

toggleClass :: Utf8 -> Dynamic Bool -> Html ()
toggleClass className (holdUniqDyn -> enableDyn) = do
  e <- ask
  initialVal <- readDyn enableDyn
  lift $ queueExp (AssignVar e.save_current_element (Var (unDomBuilder e.dom_builder_id)))
  lift $ queueExp (ElToggleClass e.dom_builder_id className initialVal)
  lift $ subscribe (updates enableDyn)
    $ queueIfAlive e.save_current_element
    . ElToggleClass (DomBuilder e.save_current_element) className

attr :: Utf8 -> Utf8 -> Html ()
attr attrName attrVal = do
  domBuilderId <- asks (.dom_builder_id)
  lift $ queueExp (ElAttr domBuilderId attrName attrVal)


text :: Utf8 -> Html ()
text contents = do
  domBuilderId <- asks (.dom_builder_id)
  lift $ queueExp (ElText domBuilderId contents)

dynText :: Dynamic Utf8 -> Html ()
dynText (holdUniqDyn -> dynContent) = do
  domBuilderId <- asks (.dom_builder_id)
  initialContent <- readDyn dynContent
  textNodeVar <- lift $ newVar
  lift $ queueExp (AssignVar textNodeVar (ElText domBuilderId initialContent))
  lift $ subscribe (updates dynContent) $
    queueIfAlive textNodeVar . ElAssignTextContent textNodeVar

dyn :: Dynamic (Html ()) -> Html ()
dyn d = do
  boundary <- insertBoundary
  finalizerNs <- lift newNamespace
  htmlEnv <- ask
  let
    setup html = do
      clearBoundary boundary
      finalizeNamespace finalizerNs
      execHtmlT htmlEnv {dom_builder_id = DomBuilder boundary} html
    applyFinalizer e = e {finalizer_ns = finalizerNs}
  lift $ performDyn $ fmap (local applyFinalizer . setup) d

insertBoundary :: Html VarId
insertBoundary = do
  domBuilderId <- asks (.dom_builder_id)
  boundary <- lift $ newVar
  lift $ queueExp (AssignVar boundary (Var (unDomBuilder domBuilderId)))
  lift $ queueExp (ElInsertBoundary (DomBuilder boundary))
  return boundary

clearBoundary :: VarId -> WA ()
clearBoundary boundary = queueExp (ElClearBoundary (DomBuilder boundary))

destroyBoundary :: VarId -> WA ()
destroyBoundary boundary = queueExp (ElDestroyBuilder (DomBuilder boundary))

simpleList
  :: forall a. Dynamic [a]
  -- ^ Some dynamic data from the above scope
  -> (Int -> DynRef a -> Html ())
  -- ^ Function to build children widget. Accepts the index inside the
  -- collection and dynamic data for that particular element
  -> Html ()
simpleList listDyn h = do
  htmlEnv <- ask
  internalStateRef <- liftIO $ newIORef ([] :: [ElemEnv a])
  boundary <- insertBoundary
  let
    setup :: Int -> [a] -> [ElemEnv a] -> Html [ElemEnv a]
    setup idx new existing = case (existing, new) of
      ([], []) -> return []
      -- New list is longer, append new elements
      ([], x:xs) -> do
        newElem <- newElemEnv x
        let
          wasmEnv = WAEnv newElem.ee_namespace
          htmlEnv = HtmlEnv newElem.ee_boundary newElem.ee_save_current_element
        local (const htmlEnv) $ monohoist (local (const wasmEnv)) $ h idx newElem.ee_dyn_ref
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
        ee_boundary <- DomBuilder <$> insertBoundary
        ee_save_current_element <- lift newVar
        return ElemEnv {..}
    finalizeElems :: Bool -> [ElemEnv a] -> Html ()
    finalizeElems remove = mapM_ \ee -> do
      when remove $ lift $ destroyBoundary (unDomBuilder ee.ee_boundary)
      lift $ finalizeNamespace ee.ee_namespace
    updateList new = do
      eenvs <- liftIO $ readIORef internalStateRef
      newEenvs <- setup 0 new eenvs
      liftIO $ writeIORef internalStateRef newEenvs
    applyBoundary e = e
      { dom_builder_id = DomBuilder boundary
      }
  lift $ performDyn $ fmap (execHtmlT htmlEnv . local applyBoundary . updateList) listDyn
  return ()
  where
    monohoist :: forall a. (forall a. WA a -> WA a) -> Html a -> Html a
    monohoist f (HtmlT (ReaderT g)) = HtmlT $ ReaderT \e -> f (g e)

-- | Auxilliary datatype that helps to implement 'simpleList'
data ElemEnv a = ElemEnv
  { ee_boundary :: DomBuilder
  , ee_save_current_element :: VarId
  , ee_dyn_ref :: DynRef a
  , ee_namespace :: FinalizerNs
  }

attachToBody :: Html a -> WA a
attachToBody html = do
  dom_builder_id <- DomBuilder <$> newVar
  save_current_element <- newVar
  queueExp $ ElInitBuilder dom_builder_id (Id "document" `Dot` "body")
  let htmlEnv = HtmlEnv {dom_builder_id, save_current_element}
  execHtmlT htmlEnv html
