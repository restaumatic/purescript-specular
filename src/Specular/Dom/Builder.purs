module Specular.Dom.Builder (
    Builder
  , runBuilder
  , local
  , unBuilder
  , mkBuilder'
  , runBuilder'
) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder)
import Specular.Dom.Node.Class (appendChild, createDocumentFragment, createTextNode, insertBefore, moveAllBetweenInclusive, parentNode, removeAllBetween)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, modifyRef, newRef, pushDelayed, readRef, sequenceEffects, unsafeFreezeDelayed, writeRef)
import Specular.Internal.RIO (RIO(..), rio, runRIO)
import Specular.Internal.RIO as RIO

newtype Builder env a = Builder (RIO (BuilderEnv env) a)

type BuilderEnv env =
  { parent :: Node
  , cleanup :: DelayedEffects
  , userEnv :: env
  }

derive newtype instance functorBuilder :: Functor (Builder env)
derive newtype instance applyBuilder :: Apply (Builder env)
derive newtype instance applicativeBuilder :: Applicative (Builder env)
derive newtype instance bindBuilder :: Bind (Builder env)
derive newtype instance monadBuilder :: Monad (Builder env)
derive newtype instance monadEffectBuilder :: MonadEffect (Builder env)

instance monadCleanupBuilder :: MonadCleanup (Builder env) where
  onCleanup action = mkBuilder $ \env -> pushDelayed env.cleanup action

instance monadAskBuilder :: MonadAsk env (Builder env) where
  ask = _.userEnv <$> getEnv

instance monadReaderBuilder :: MonadReader env (Builder env) where
  local = local

local :: forall e r a. (e -> r) -> Builder r a -> Builder e a
local fn (Builder x) = Builder $ RIO.local (\env -> env { userEnv = fn env.userEnv }) x

mkBuilder' :: forall env a. (EffectFn1 (BuilderEnv env) a) -> Builder env a
mkBuilder' = Builder <<< RIO

mkBuilder :: forall env a. (BuilderEnv env -> Effect a) -> Builder env a
mkBuilder = Builder <<< rio

unBuilder :: forall env a. Builder env a -> RIO (BuilderEnv env) a
unBuilder (Builder f) = f

runBuilder' :: forall env a. EffectFn2 (BuilderEnv env) (Builder env a) a
runBuilder' = mkEffectFn2 \env (Builder (RIO f)) -> runEffectFn1 f env

runBuilder :: forall a. Node -> Builder Unit a -> Effect (Tuple a (Effect Unit))
runBuilder = runBuilderWithUserEnv unit

runBuilderWithUserEnv :: forall env a. env -> Node -> Builder env a -> Effect (Tuple a (Effect Unit))
runBuilderWithUserEnv userEnv parent (Builder f) = do
  actionsMutable <- emptyDelayed
  let env = { parent, cleanup: actionsMutable, userEnv }
  result <- runRIO env f
  actions <- unsafeFreezeDelayed actionsMutable
  pure (Tuple result (sequenceEffects actions))

getEnv :: forall env. Builder env (BuilderEnv env)
getEnv = Builder ask

setParent :: forall env. Node -> BuilderEnv env -> BuilderEnv env
setParent parent env = env { parent = parent }

instance monadReplaceBuilder :: MonadReplace (Builder env) where

  newSlot = do
    env <- getEnv

    placeholderAfter <- liftEffect $ createTextNode ""
    liftEffect $ appendChild placeholderAfter env.parent
    -- FIXME: placeholderAfter leaks if replace is never called

    cleanupRef <- liftEffect $ newRef (mempty :: Effect Unit)

    let
      replace :: forall a. Builder env a -> Effect a
      replace inner = do
        fragment <- createDocumentFragment
        Tuple result cleanup <- runBuilderWithUserEnv env.userEnv fragment inner
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

      append :: Effect (Slot (Builder env))
      append = do
        fragment <- createDocumentFragment
        Tuple slot cleanup <- runBuilderWithUserEnv env.userEnv fragment newSlot
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

instance monadDomBuilderBuilder :: MonadDomBuilder (Builder env) where
  liftBuilder fn = Builder (RIO fn)
  liftBuilderWithRun fn =
    Builder $ rio \env ->
      runEffectFn2 fn env (mkEffectFn2 \env' (Builder (RIO m)) -> runEffectFn1 m env')

instance monadDetachBuilder :: MonadDetach (Builder env) where
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
