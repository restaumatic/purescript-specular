module Control.Monad.Cleanup where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Tuple (Tuple(..), snd)
import Specular.Internal.Effect (DelayedEffects, emptyDelayed, pushDelayed, sequenceEffects, unsafeFreezeDelayed)

class Monad m <= MonadCleanup m where
  -- | Add a cleanup action.
  onCleanup :: Effect Unit -> m Unit


newtype CleanupT :: forall k. (k -> Type) -> k -> Type
newtype CleanupT m a = CleanupT (ReaderT DelayedEffects m a)

derive newtype instance functorCleanupT :: Functor m => Functor (CleanupT m)
derive newtype instance applyCleanupT :: Apply m => Apply (CleanupT m)
derive newtype instance applicativeCleanupT :: Applicative m => Applicative (CleanupT m)
derive newtype instance bindCleanupT :: Bind m => Bind (CleanupT m)
derive newtype instance monadCleanupT :: Monad m => Monad (CleanupT m)
derive newtype instance monadEffectCleanupT :: MonadEffect m => MonadEffect (CleanupT m)

runCleanupT :: forall m a. MonadEffect m => CleanupT m a -> m (Tuple a (Effect Unit))
runCleanupT (CleanupT m) = do
  actionsMutable <- liftEffect emptyDelayed
  result <- runReaderT m actionsMutable
  actions <- liftEffect $ unsafeFreezeDelayed actionsMutable
  pure (Tuple result (sequenceEffects actions))

execCleanupT :: forall m. MonadEffect m => CleanupT m Unit -> m (Effect Unit)
execCleanupT = map snd <<< runCleanupT

instance monadTransCleanupT :: MonadTrans CleanupT where
  lift = CleanupT <<< lift

instance monadCleanupCleanupT :: MonadEffect m => MonadCleanup (CleanupT m) where
  onCleanup action = CleanupT $ ReaderT $ \actions -> liftEffect $ pushDelayed actions action

instance monadCleanupReaderT :: MonadCleanup m => MonadCleanup (ReaderT r m) where
  onCleanup = lift <<< onCleanup
