module Specular.Dom.Builder (
    Builder
  , runBuilder
) where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.RIO (RIO, rio, runRIO)
import Control.Monad.RIO as RIO
import Control.Monad.Reader (ask)
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
import Specular.Dom.Node.Class (class DOM, appendChild, appendRawHtml, createDocumentFragment, createElementNS, createTextNode, insertBefore, moveAllBetweenInclusive, parentNode, removeAllBetween, removeAttributes, setAttributes, setText)
import Specular.FRP (class MonadHold, class MonadHost, class MonadHostCreate, class MonadPull, foldDynImpl, foldDynMaybeImpl, newBehavior, newEvent, pull)
import Specular.FRP.Base (subscribeEvent_Impl)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)

newtype Builder node a = Builder (RIO (BuilderEnv node) a)

type BuilderEnv node =
  { parent :: node
  , cleanup :: DelayedEffects
  }

derive newtype instance functorBuilder :: Functor (Builder node)
derive newtype instance applyBuilder :: Apply (Builder node)
derive newtype instance applicativeBuilder :: Applicative (Builder node)
derive newtype instance bindBuilder :: Bind (Builder node)
derive newtype instance monadBuilder :: Monad (Builder node)
derive newtype instance monadEffBuilder :: MonadEff eff (Builder node)
derive newtype instance monadIOSyncBuilder :: MonadIOSync (Builder node)

instance monadCleanupBuilder :: MonadCleanup (Builder node) where
  onCleanup action = mkBuilder $ \env -> DE.push env.cleanup action

mkBuilder :: forall node a. (BuilderEnv node -> IOSync a) -> Builder node a
mkBuilder = Builder <<< rio

unBuilder :: forall node a. Builder node a -> RIO (BuilderEnv node) a
unBuilder (Builder f) = f

runBuilder :: forall node a. node -> Builder node a -> IOSync (Tuple a (IOSync Unit))
runBuilder parent (Builder f) = do
  actionsMutable <- DE.empty
  let env = { parent, cleanup: actionsMutable }
  result <- runRIO env f
  actions <- DE.unsafeFreeze actionsMutable
  pure (Tuple result (DE.sequenceEffects actions))

getEnv :: forall node. Builder node (BuilderEnv node)
getEnv = Builder ask

setParent :: forall node. node -> BuilderEnv node -> BuilderEnv node
setParent parent env = env { parent = parent }

instance monadReplaceBuilder :: DOM node => MonadReplace (Builder node) where

  newSlot = do
    env <- getEnv

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter env.parent
    -- FIXME: placeholderAfter leaks if replace is never called

    cleanupRef <- liftIOSync $ newIORef (mempty :: IOSync Unit)

    let
      replace :: forall a. Builder node a -> IOSync a
      replace inner = do
        fragment <- createDocumentFragment
        Tuple result cleanup <- runBuilder fragment inner
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

      append :: IOSync (Slot (Builder node))
      append = do
        fragment <- createDocumentFragment
        Tuple slot cleanup <- runBuilder fragment newSlot
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

instance monadHoldBuilder :: MonadHold (Builder node) where
  foldDyn = foldDynImpl
  foldDynMaybe = foldDynMaybeImpl

instance monadPullBuilder :: MonadPull (Builder node) where
  pull = liftIOSync <<< pull

instance monadHostCreateBuilder :: MonadHostCreate (Builder node) where
  newEvent = liftIOSync newEvent
  newBehavior = liftIOSync <<< newBehavior

instance monadHostBuilder :: MonadHost (Builder node) where
  subscribeEvent_ = subscribeEvent_Impl

instance monadDomBuilderBuilder :: DOM node => MonadDomBuilder node (Builder node) where

  text str = mkBuilder \env -> do
    node <- createTextNode str
    appendChild node env.parent

  dynText dstr = do
    node <- mkBuilder \env -> do
      node <- createTextNode ""
      appendChild node env.parent
      pure node
    subscribeWeakDyn_ (setText node) dstr

  rawHtml html = mkBuilder \env -> 
    appendRawHtml html env.parent

  elDynAttrNS' namespace tagName dynAttrs inner = do
    env <- getEnv
    node <- liftIOSync $ createElementNS namespace tagName

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
    result <- Builder $ RIO.local (setParent node) $ unBuilder inner
    liftIOSync $ appendChild node env.parent
    pure (Tuple node result)

  elAttr tagName attrs inner = do
    env <- getEnv
    node <- liftIOSync $ createElementNS Nothing tagName
    liftIOSync $ setAttributes node attrs
    result <- Builder $ RIO.local (setParent node) $ unBuilder inner
    liftIOSync $ appendChild node env.parent
    pure result

instance monadDetachBuilder :: DOM node => MonadDetach (Builder node) where
  detach inner = do
    fragment <- liftIOSync createDocumentFragment

    placeholderBefore <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderBefore fragment

    result <- Builder $ RIO.local (setParent fragment) $ unBuilder inner

    placeholderAfter <- liftIOSync $ createTextNode ""
    liftIOSync $ appendChild placeholderAfter fragment

    let
      attach = mkBuilder $ \env ->
        moveAllBetweenInclusive placeholderBefore placeholderAfter env.parent

    pure { value: result, widget: attach }
