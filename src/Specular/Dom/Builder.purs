module Specular.Dom.Builder (
    Builder
  , runBuilder
) where

import Prelude

import Control.Monad.Aff (killFiber, launchAff, launchAff_)
import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.IO (IO, runIO)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Replace (class MonadReplace, runReplaceable)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Array as A
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..), snd)
import Specular.Dom.Builder.Class (class MonadDomBuilder)
import Specular.Dom.Node.Class (class DOM, class EventDOM, Attrs, EventType, addEventListener, appendChild, createDocumentFragment, createElement, createTextNode, insertBefore, parentNode, removeAllBetween, removeAttributes, setAttributes)
import Specular.FRP (class MonadHold, class MonadHost, class MonadHostCreate, class MonadPull, Dynamic, Event, WeakDynamic, foldDyn, newBehavior, newEvent, pull, subscribeDyn_, subscribeEvent_, subscribeWeakDyn_)

newtype Builder node a = Builder (ReaderT (BuilderEnv node) (WriterT (IOSync Unit) IOSync) a)

derive newtype instance functorBuilder :: Functor (Builder node)
derive newtype instance applyBuilder :: Apply (Builder node)
derive newtype instance applicativeBuilder :: Applicative (Builder node)
derive newtype instance bindBuilder :: Bind (Builder node)
derive newtype instance monadBuilder :: Monad (Builder node)
derive newtype instance monadEffBuilder :: MonadEff eff (Builder node)
derive newtype instance monadIOSyncBuilder :: MonadIOSync (Builder node)

unBuilder :: forall node a. Builder node a -> ReaderT (BuilderEnv node) (WriterT (IOSync Unit) IOSync) a
unBuilder (Builder f) = f

runBuilder :: forall node a. BuilderEnv node -> Builder node a -> IOSync (Tuple a (IOSync Unit))
runBuilder env (Builder f) = runWriterT $ runReaderT f env

type BuilderEnv node = { parent :: node }

getEnv :: forall node. Builder node (BuilderEnv node)
getEnv = Builder ask

setParent :: forall node. node -> BuilderEnv node -> BuilderEnv node
setParent parent env = env { parent = parent }

instance monadCleanupBuilder :: MonadCleanup (Builder node) where
  onCleanup action = Builder $ tell action

instance monadReplaceBuilder :: DOM node => MonadReplace (Builder node) where
  runReplaceable initial = do
    env <- getEnv

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter env.parent

    cleanupRef <- liftIOSync $ newIORef mempty

    let
      replaceWith inner = do
        join $ readIORef cleanupRef

        fragment <- createDocumentFragment
        Tuple _ innerCleanup <- runBuilder { parent: fragment } inner

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            placeholderBefore <- createTextNode ""
            insertBefore placeholderBefore placeholderAfter parent
            insertBefore fragment placeholderAfter parent

            writeIORef cleanupRef $ do
              innerCleanup
              removeAllBetween placeholderBefore placeholderAfter
              writeIORef cleanupRef mempty -- TODO: explain this

          Nothing ->
            -- we've been removed from the DOM
            writeIORef cleanupRef innerCleanup

    onCleanup $ join $ readIORef cleanupRef

    pure { replace: replaceWith }


liftCleanupT :: forall node a. CleanupT IOSync a -> Builder node a
liftCleanupT action = do
  Tuple result cleanup <- liftIOSync $ runCleanupT action
  onCleanup cleanup
  pure result

instance monadHoldBuilder :: MonadHold (Builder node) where
  foldDyn f x0 e = liftCleanupT $ foldDyn f x0 e

instance monadPullBuilder :: MonadPull (Builder node) where
  pull = liftIOSync <<< pull

instance monadHostCreateBuilder :: MonadHostCreate IOSync (Builder node) where
  newEvent = liftIOSync newEvent
  newBehavior = liftIOSync <<< newBehavior

instance monadHostBuilder :: MonadHost IOSync (Builder node) where
  subscribeEvent_ handler e = liftCleanupT $ subscribeEvent_ handler e
  hostEffect = liftIOSync

instance monadDomBuilderBuilder :: DOM node => MonadDomBuilder node (Builder node) where
  text str = do
    env <- getEnv
    liftIOSync $ do
      node <- createTextNode str
      appendChild node env.parent

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
    result <- Builder $ local (setParent node) (unBuilder inner)
    liftIOSync $ appendChild node env.parent
    pure (Tuple node result)
