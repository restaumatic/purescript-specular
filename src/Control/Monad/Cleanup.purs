module Control.Monad.Cleanup where

import Prelude

import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.DelayedEffects (DelayedEffects)
import Data.DelayedEffects as DE
import Data.Tuple (Tuple(..), snd)

class Monad m <= MonadCleanup m where
  -- | Add a cleanup action.
  onCleanup :: IOSync Unit -> m Unit


newtype CleanupT m a = CleanupT (ReaderT DelayedEffects m a)

derive newtype instance functorCleanupT :: Functor m => Functor (CleanupT m)
derive newtype instance applyCleanupT :: Apply m => Apply (CleanupT m)
derive newtype instance applicativeCleanupT :: Applicative m => Applicative (CleanupT m)
derive newtype instance bindCleanupT :: Bind m => Bind (CleanupT m)
derive newtype instance monadCleanupT :: Monad m => Monad (CleanupT m)
derive newtype instance monadIOSyncCleanupT :: MonadIOSync m => MonadIOSync (CleanupT m)

runCleanupT :: forall m a. MonadIOSync m => CleanupT m a -> m (Tuple a (IOSync Unit))
runCleanupT (CleanupT m) = do
  actionsMutable <- liftIOSync DE.empty
  result <- runReaderT m actionsMutable
  actions <- liftIOSync $ DE.unsafeFreeze actionsMutable
  pure (Tuple result (DE.sequenceEffects actions))

execCleanupT :: forall m. MonadIOSync m => CleanupT m Unit -> m (IOSync Unit)
execCleanupT = map snd <<< runCleanupT

instance monadTransCleanupT :: MonadTrans CleanupT where
  lift = CleanupT <<< lift

instance monadCleanupCleanupT :: MonadIOSync m => MonadCleanup (CleanupT m) where
  onCleanup action = CleanupT $ ReaderT $ \actions -> liftIOSync $ DE.push actions action

instance monadCleanupReaderT :: MonadCleanup m => MonadCleanup (ReaderT r m) where
  onCleanup = lift <<< onCleanup
