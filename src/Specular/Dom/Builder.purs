module Specular.Dom.Builder
  ( Builder
  , runBuilder
  , local
  , unBuilder
  , mkBuilder'
  , mkBuilder
  , runBuilder'
  , getParentNode
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Replace (class MonadReplace, Slot(Slot), newSlot)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn2, runEffectFn1, runEffectFn2)
import Specular.Dom.Builder.Class (class MonadDomBuilder)
import Specular.Dom.Browser (Node, appendChild, createDocumentFragment, createTextNode, insertBefore, parentNode, removeAllBetween, removeNode)
import Effect.Ref (modify_, new, read, write)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, pushDelayed, sequenceEffects, unsafeFreezeDelayed)
import Specular.Internal.RIO (RIO(..), rio, runRIO)
import Specular.Internal.RIO as RIO
import Specular.Profiling as Profiling

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

getParentNode :: forall env. Builder env Node
getParentNode = Builder (asks _.parent)

instance MonadReplace (Builder env) where

  newSlot = do
    env <- getEnv

    placeholderBefore <- liftEffect $ createTextNode ""
    placeholderAfter <- liftEffect $ createTextNode ""
    liftEffect $ appendChild placeholderBefore env.parent
    liftEffect $ appendChild placeholderAfter env.parent

    cleanupRef <- liftEffect $ new (mempty :: Effect Unit)

    let
      replace :: forall a. Builder env a -> Effect a
      replace inner = Profiling.measure "slot replace" do
        Profiling.measure "slot remove DOM" do
          removeAllBetween placeholderBefore placeholderAfter

        fragment <- createDocumentFragment
        Tuple result cleanup <- Profiling.measure "slot init" do
          runBuilderWithUserEnv env.userEnv fragment inner
        join $ read cleanupRef

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            insertBefore fragment placeholderAfter parent

            write
              ( Profiling.measure "slot cleanup" do
                  cleanup
                  write mempty cleanupRef -- TODO: explain this
              )
              cleanupRef

          Nothing ->
            -- we've been removed from the DOM
            write cleanup cleanupRef

        pure result

      destroy :: Effect Unit
      destroy = do
        removeAllBetween placeholderBefore placeholderAfter
        removeNode placeholderBefore
        removeNode placeholderAfter
        join $ read cleanupRef

      append :: Effect (Slot (Builder env))
      append = do
        fragment <- createDocumentFragment
        Tuple slot cleanup <- runBuilderWithUserEnv env.userEnv fragment newSlot
        modify_ (_ *> cleanup) cleanupRef -- FIXME: memory leak if the inner slot is destroyed

        m_parent <- parentNode placeholderAfter

        case m_parent of
          Just parent -> do
            insertBefore fragment placeholderAfter parent
          Nothing ->
            pure unit -- FIXME

        pure slot

    onCleanup $ join $ read cleanupRef

    pure $ Slot replace destroy append

instance MonadDomBuilder (Builder env) where
  liftBuilder fn = Builder (RIO fn)
  liftBuilderWithRun fn =
    Builder $ rio \env ->
      runEffectFn2 fn env (mkEffectFn2 \env' (Builder (RIO m)) -> runEffectFn1 m env')

instance Semigroup a => Semigroup (Builder node a) where
  append = lift2 append

instance Monoid a => Monoid (Builder node a) where
  mempty = pure mempty
