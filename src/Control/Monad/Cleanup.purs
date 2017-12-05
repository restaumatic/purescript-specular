module Control.Monad.Cleanup where

import Prelude

import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Writer.Trans (execWriterT, runWriterT)
import Data.Tuple (Tuple)

class Monad m <= MonadCleanup m where
  -- | Add a cleanup action.
  onCleanup :: IOSync Unit -> m Unit

newtype CleanupT m a = CleanupT (WriterT (IOSync Unit) m a)

derive newtype instance functorCleanupT :: Functor m => Functor (CleanupT m)
derive newtype instance applyCleanupT :: Apply m => Apply (CleanupT m)
derive newtype instance applicativeCleanupT :: Applicative m => Applicative (CleanupT m)
derive newtype instance bindCleanupT :: Bind m => Bind (CleanupT m)
derive newtype instance monadCleanupT :: Monad m => Monad (CleanupT m)
derive newtype instance monadIOSyncCleanupT :: MonadIOSync m => MonadIOSync (CleanupT m)

runCleanupT :: forall m a. CleanupT m a -> m (Tuple a (IOSync Unit))
runCleanupT (CleanupT w) = runWriterT w

execCleanupT :: forall m. Functor m => CleanupT m Unit -> m (IOSync Unit)
execCleanupT (CleanupT w) = execWriterT w

instance monadCleanupCleanupT :: Monad m => MonadCleanup (CleanupT m) where
  onCleanup = CleanupT <<< tell

instance monadCleanupReaderT :: MonadCleanup m => MonadCleanup (ReaderT r m) where
  onCleanup = lift <<< onCleanup
