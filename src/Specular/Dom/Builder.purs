module Specular.Dom.Builder (
    BuilderT
  , runBuilderT
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as A
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(Tuple))
import Specular.Dom.Builder.Class (class MonadDomBuilder)
import Specular.Dom.Node.Class (class DOM, appendChild, appendRawHtml, createDocumentFragment, createElement, createTextNode, insertBefore, parentNode, removeAllBetween, removeAttributes, setAttributes, setText)
import Specular.FRP (class MonadFRP, class MonadHold, class MonadHost, class MonadHostCreate, class MonadPull, foldDyn, hostEffect, newBehavior, newEvent, pull, subscribeEvent_)
import Specular.FRP.Base (foldDynMaybe)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)

newtype BuilderT node m a = BuilderT (ReaderT (BuilderEnv node) m a)

type Builder node = BuilderT node (CleanupT IOSync)

derive newtype instance functorBuilderT :: Functor m => Functor (BuilderT node m)
derive newtype instance applyBuilderT :: Apply m => Apply (BuilderT node m)
derive newtype instance applicativeBuilderT :: Applicative m => Applicative (BuilderT node m)
derive newtype instance bindBuilderT :: Bind m => Bind (BuilderT node m)
derive newtype instance monadBuilderT :: Monad m => Monad (BuilderT node m)
derive newtype instance monadEffBuilderT :: MonadEff eff m => MonadEff eff (BuilderT node m)
derive newtype instance monadIOSyncBuilderT :: MonadIOSync m => MonadIOSync (BuilderT node m)
derive newtype instance monadCleanupBuilderT :: MonadCleanup m => MonadCleanup (BuilderT node m)
derive newtype instance monadTransBuilderT :: MonadTrans (BuilderT node)

unBuilderT :: forall node m a. BuilderT node m a -> ReaderT (BuilderEnv node) m a
unBuilderT (BuilderT f) = f

runBuilder :: forall node a. BuilderEnv node -> Builder node a -> IOSync (Tuple a (IOSync Unit))
runBuilder env (BuilderT f) = runCleanupT $ runReaderT f env

runBuilderT :: forall node m a. BuilderEnv node -> BuilderT node m a -> m a
runBuilderT env (BuilderT f) = runReaderT f env

type BuilderEnv node = { parent :: node }

getEnv :: forall node m. Monad m => BuilderT node m (BuilderEnv node)
getEnv = BuilderT ask

setParent :: forall node. node -> BuilderEnv node -> BuilderEnv node
setParent parent env = env { parent = parent }

instance monadReplaceBuilderT :: DOM node
    => MonadReplace (BuilderT node (CleanupT IOSync)) where

  newSlot = do
    env <- getEnv

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter env.parent
    -- FIXME: placeholderAfter leaks if replace is never called

    cleanupRef <- liftIOSync $ newIORef (mempty :: IOSync Unit)

    let
      replace :: forall a. BuilderT node (CleanupT IOSync) a -> IOSync a
      replace inner = do
        fragment <- createDocumentFragment
        Tuple result cleanup <- runCleanupT $ runBuilderT { parent: fragment } inner
        join $ readIORef cleanupRef

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            placeholderBefore <- createTextNode ""
            insertBefore placeholderBefore placeholderAfter parent
            insertBefore fragment placeholderAfter parent

            writeIORef cleanupRef $ do
              cleanup
              removeAllBetween placeholderBefore placeholderAfter
              writeIORef cleanupRef mempty -- TODO: explain this

          Nothing ->
            -- we've been removed from the DOM
            writeIORef cleanupRef cleanup

        pure result

      destroy :: IOSync Unit
      destroy = do
        join $ readIORef cleanupRef

      append :: IOSync (Slot (BuilderT node (CleanupT IOSync)))
      append = do
        fragment <- createDocumentFragment
        Tuple slot cleanup <- runCleanupT $ runBuilderT { parent: fragment } newSlot
        modifyIORef cleanupRef (_ *> cleanup) -- FIXME: memory leak if the inner slot is destroyed

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            insertBefore fragment placeholderAfter parent
          Nothing ->
            pure unit -- FIXME

        pure slot

    onCleanup $ join $ readIORef cleanupRef

    pure $ Slot { replace, destroy, append }

instance monadHoldBuilderT :: MonadHold m => MonadHold (BuilderT node m) where
  foldDyn f x0 e = lift $ foldDyn f x0 e
  foldDynMaybe f x0 e = lift $ foldDynMaybe f x0 e

instance monadPullBuilderT :: MonadPull m => MonadPull (BuilderT node m) where
  pull = lift <<< pull

instance monadHostCreateBuilderT :: (Monad m, MonadHostCreate io m)
    => MonadHostCreate io (BuilderT node m) where
  newEvent = lift newEvent
  newBehavior = lift <<< newBehavior

instance monadHostBuilder :: (Monad io, MonadHost io m)
    => MonadHost io (BuilderT node m) where
  subscribeEvent_ handler e = lift $ subscribeEvent_ handler e
  hostEffect = lift <<< hostEffect

instance monadDomBuilderBuilder :: (MonadIOSync m, MonadFRP m, DOM node)
    => MonadDomBuilder node (BuilderT node m) where

  text str = do
    env <- getEnv
    liftIOSync $ do
      node <- createTextNode str
      appendChild node env.parent

  dynText dstr = do
    env <- getEnv
    node <- liftIOSync $ do
      node <- createTextNode ""
      appendChild node env.parent
      pure node
    subscribeWeakDyn_ (setText node) dstr

  rawHtml html = do
    env <- getEnv
    liftIOSync $ appendRawHtml html env.parent

  elDynAttr' tagName dynAttrs inner = do
    env <- getEnv
    node <- liftIOSync $ createElement tagName

    attrsRef <- liftIOSync $ newIORef mempty
    let
      resetAttributes newAttrs = do
        oldAttrs <- readIORef attrsRef
        writeIORef attrsRef newAttrs
        let
          changed = SM.filterWithKey (\k v -> SM.lookup k oldAttrs /= Just v) newAttrs
          removed = A.filter (\k -> not (k `SM.member` newAttrs)) $ SM.keys oldAttrs

        removeAttributes node removed
        setAttributes node changed

    subscribeWeakDyn_ resetAttributes dynAttrs
    result <- BuilderT $ local (setParent node) (unBuilderT inner)
    liftIOSync $ appendChild node env.parent
    pure (Tuple node result)
