module Specular.Dom.Builder (
    BuilderT
  , runBuilderT
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.RIO (RIO(..), rio, runRIO)
import Control.Monad.RIO as RIO
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Data.Array as A
import Data.DelayedEffects (DelayedEffects)
import Data.DelayedEffects as DE
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(Tuple))
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder)
import Specular.Dom.Node.Class (class DOM, appendChild, appendRawHtml, createDocumentFragment, createElement, createTextNode, insertBefore, moveAllBetweenInclusive, parentNode, removeAllBetween, removeAttributes, setAttributes, setText)
import Specular.FRP (class MonadFRP, class MonadHold, class MonadHost, class MonadHostCreate, class MonadPull, foldDyn, foldDynImpl, foldDynMaybeImpl, hostEffect, newBehavior, newEvent, pull, subscribeEvent_)
import Specular.FRP.Base (foldDynMaybe, subscribeEvent_Impl)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)

newtype BuilderT node (m :: Type -> Type) a = BuilderT (RIO (BuilderEnv node) a)

type Builder node = BuilderT node (CleanupT IOSync)

lift :: forall node m a. IOSync a -> BuilderT node m a
lift = liftIOSync

mkBuilder :: forall node m a. (BuilderEnv node -> IOSync a) -> BuilderT node m a
mkBuilder = BuilderT <<< rio

derive newtype instance functorBuilderT :: Functor (BuilderT node m)
derive newtype instance applyBuilderT :: Apply (BuilderT node m)
derive newtype instance applicativeBuilderT :: Applicative (BuilderT node m)
derive newtype instance bindBuilderT :: Bind (BuilderT node m)
derive newtype instance monadBuilderT :: Monad (BuilderT node m)
derive newtype instance monadEffBuilderT :: MonadEff eff (BuilderT node m)
derive newtype instance monadIOSyncBuilderT :: MonadIOSync (BuilderT node m)
instance monadCleanupBuilderT :: MonadCleanup (BuilderT node m) where
  onCleanup action = mkBuilder $ \env -> DE.push env.cleanup action

unBuilderT :: forall node m a. BuilderT node m a -> RIO (BuilderEnv node) a
unBuilderT (BuilderT f) = f

runBuilderT :: forall node m a. node -> BuilderT node m a -> IOSync (Tuple a (IOSync Unit))
runBuilderT parent (BuilderT f) = do
  actionsMutable <- DE.empty
  let env = { parent, cleanup: actionsMutable }
  result <- runRIO env f
  actions <- DE.unsafeFreeze actionsMutable
  pure (Tuple result (DE.sequenceEffects actions))

type BuilderEnv node =
  { parent :: node
  , cleanup :: DelayedEffects
  }

getEnv :: forall node m. BuilderT node m (BuilderEnv node)
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
        Tuple result cleanup <- runBuilderT fragment inner
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
        Tuple slot cleanup <- runBuilderT fragment newSlot
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

instance monadHoldBuilderT :: MonadHold (BuilderT node m) where
  foldDyn = foldDynImpl
  foldDynMaybe = foldDynMaybeImpl

instance monadPullBuilderT :: MonadPull (BuilderT node m) where
  pull = liftIOSync <<< pull

instance monadHostCreateBuilderT :: MonadHostCreate IOSync (BuilderT node m) where
  newEvent = lift newEvent
  newBehavior = lift <<< newBehavior

instance monadHostBuilder :: MonadHost IOSync (BuilderT node m) where
  subscribeEvent_ = subscribeEvent_Impl
  hostEffect = liftIOSync

instance monadDomBuilderBuilder :: DOM node => MonadDomBuilder node (BuilderT node m) where

  text str = mkBuilder $ \env -> do
    node <- createTextNode str
    appendChild node env.parent

  dynText dstr = do
    node <- mkBuilder $ \env -> do
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
    result <- BuilderT $ RIO.local (setParent node) $ unBuilderT inner
    liftIOSync $ appendChild node env.parent
    pure (Tuple node result)

instance monadDetachBuilder :: DOM node => MonadDetach (BuilderT node m) where
  detach inner = do
    fragment <- liftIOSync createDocumentFragment

    placeholderBefore <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderBefore fragment

    result <- BuilderT $ RIO.local (setParent fragment) $ unBuilderT inner

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter fragment

    let
      attach = mkBuilder $ \env ->
        moveAllBetweenInclusive placeholderBefore placeholderAfter env.parent

    pure { value: result, widget: attach }
