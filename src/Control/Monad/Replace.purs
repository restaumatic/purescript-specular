module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Tuple (Tuple(..))

class (Monad m, MonadCleanup m) <= MonadReplace m where
  runReplaceable :: m Unit -> m { replace :: m Unit -> IOSync Unit }

instance monadReplaceCleanupTIOSync :: MonadReplace (CleanupT IOSync) where
  runReplaceable initial = do
    Tuple _ cleanup <- liftIOSync $ runCleanupT initial
    cleanupRef <- liftIOSync $ newIORef cleanup

    let
      replace inner = do
        join $ readIORef cleanupRef
        Tuple _ cleanup <- runCleanupT inner
        writeIORef cleanupRef cleanup

    onCleanup $ join $ readIORef cleanupRef

    pure { replace }
