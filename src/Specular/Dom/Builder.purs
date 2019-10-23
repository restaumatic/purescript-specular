module Specular.Dom.Builder (
    Builder
  , runBuilder
  , unBuilder
  , mkBuilder'
  , runBuilder'
) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Reader (ask)
import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Foreign.Object as SM
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder)
import Specular.Dom.Node.Class (appendChild, appendRawHtml, createDocumentFragment, createElementNS, createTextNode, insertBefore, moveAllBetweenInclusive, parentNode, removeAllBetween, removeAttributes, setAttributes, setText)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, modifyRef, newRef, pushDelayed, readRef, sequenceEffects, unsafeFreezeDelayed, writeRef)
import Specular.Internal.RIO (RIO(..), rio, runRIO)
import Specular.Internal.RIO as RIO

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
derive newtype instance monadEffectBuilder :: MonadEffect (Builder node)

instance monadCleanupBuilder :: MonadCleanup (Builder node) where
  onCleanup action = mkBuilder $ \env -> pushDelayed env.cleanup action

mkBuilder' :: forall node a. (EffectFn1 (BuilderEnv node) a) -> Builder node a
mkBuilder' = Builder <<< RIO

mkBuilder :: forall node a. (BuilderEnv node -> Effect a) -> Builder node a
mkBuilder = Builder <<< rio

unBuilder :: forall node a. Builder node a -> RIO (BuilderEnv node) a
unBuilder (Builder f) = f

runBuilder' :: forall node a. EffectFn2 (BuilderEnv node) (Builder node a) a
runBuilder' = mkEffectFn2 \env (Builder (RIO f)) -> runEffectFn1 f env

runBuilder :: forall node a. node -> Builder node a -> Effect (Tuple a (Effect Unit))
runBuilder parent (Builder f) = do
  actionsMutable <- emptyDelayed
  let env = { parent, cleanup: actionsMutable }
  result <- runRIO env f
  actions <- unsafeFreezeDelayed actionsMutable
  pure (Tuple result (sequenceEffects actions))

getEnv :: forall node. Builder node (BuilderEnv node)
getEnv = Builder ask

setParent :: forall node. node -> BuilderEnv node -> BuilderEnv node
setParent parent env = env { parent = parent }

instance monadReplaceBuilder :: MonadReplace (Builder Node) where

  newSlot = do
    env <- getEnv

    placeholderAfter <- liftEffect $ createTextNode ""
    liftEffect $ appendChild placeholderAfter env.parent
    -- FIXME: placeholderAfter leaks if replace is never called

    cleanupRef <- liftEffect $ newRef (mempty :: Effect Unit)

    let
      replace :: forall a. Builder Node a -> Effect a
      replace inner = do
        fragment <- createDocumentFragment
        Tuple result cleanup <- runBuilder fragment inner
        join $ readRef cleanupRef

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            placeholderBefore <- createTextNode ""
            insertBefore placeholderBefore placeholderAfter parent
            insertBefore fragment placeholderAfter parent

            writeRef cleanupRef $ do
              cleanup
              removeAllBetween placeholderBefore placeholderAfter
              writeRef cleanupRef mempty -- TODO: explain this

          Nothing ->
            -- we've been removed from the DOM
            writeRef cleanupRef cleanup

        pure result

      destroy :: Effect Unit
      destroy = do
        join $ readRef cleanupRef

      append :: Effect (Slot (Builder Node))
      append = do
        fragment <- createDocumentFragment
        Tuple slot cleanup <- runBuilder fragment newSlot
        modifyRef cleanupRef (_ *> cleanup) -- FIXME: memory leak if the inner slot is destroyed

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            insertBefore fragment placeholderAfter parent
          Nothing ->
            pure unit -- FIXME

        pure slot

    onCleanup $ join $ readRef cleanupRef

    pure $ Slot { replace, destroy, append }

instance monadDomBuilderBuilder :: MonadDomBuilder (Builder Node) where

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
    node <- liftEffect $ createElementNS namespace tagName

    attrsRef <- liftEffect $ newRef mempty
    let
      resetAttributes newAttrs = do
        oldAttrs <- readRef attrsRef
        writeRef attrsRef newAttrs
        let
          changed = SM.filterWithKey (\k v -> SM.lookup k oldAttrs /= Just v) newAttrs
          removed = A.filter (\k -> not (k `SM.member` newAttrs)) $ SM.keys oldAttrs

        removeAttributes node removed
        setAttributes node changed

    subscribeWeakDyn_ resetAttributes dynAttrs
    result <- Builder $ RIO.local (setParent node) $ unBuilder inner
    liftEffect $ appendChild node env.parent
    pure (Tuple node result)

  elAttr tagName attrs inner = do
    env <- getEnv
    node <- liftEffect $ createElementNS Nothing tagName
    liftEffect $ setAttributes node attrs
    result <- Builder $ RIO.local (setParent node) $ unBuilder inner
    liftEffect $ appendChild node env.parent
    pure result

  liftBuilder fn = Builder (RIO fn)
  liftBuilderWithRun fn =
    Builder $ rio \env ->
      runEffectFn2 fn env (mkEffectFn2 \env' (Builder (RIO m)) -> runEffectFn1 m env')

instance monadDetachBuilder :: MonadDetach (Builder Node) where
  detach inner = do
    fragment <- liftEffect createDocumentFragment

    placeholderBefore <- liftEffect $ createTextNode ""
    liftEffect $ appendChild placeholderBefore fragment

    result <- Builder $ RIO.local (setParent fragment) $ unBuilder inner

    placeholderAfter <- liftEffect $ createTextNode ""
    liftEffect $ appendChild placeholderAfter fragment

    let
      attach = mkBuilder $ \env ->
        moveAllBetweenInclusive placeholderBefore placeholderAfter env.parent

    pure { value: result, widget: attach }

instance semigroupBuilder :: Semigroup a => Semigroup (Builder node a) where
  append = lift2 append

instance monoidBuilder :: Monoid a => Monoid (Builder node a) where
  mempty = pure mempty
