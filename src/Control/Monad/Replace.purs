module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

class (Monad m, MonadCleanup m) <= MonadReplace m where
  runReplaceable :: forall a. m { replace :: m a -> IOSync a }

instance monadReplaceCleanupTIOSync :: MonadReplace (CleanupT IOSync) where
  runReplaceable = do
    cleanupRef <- liftIOSync $ newIORef (mempty :: IOSync Unit)

    let
      replace inner = do
        join $ readIORef cleanupRef
        Tuple result cleanup <- runCleanupT inner
        writeIORef cleanupRef cleanup
        pure result

    onCleanup $ join $ readIORef cleanupRef

    pure { replace }
