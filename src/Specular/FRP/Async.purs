module Specular.FRP.Async where

import Prelude

import Control.Monad.Aff (killFiber, launchAff, launchAff_)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.IO (IO, runIO)
import Control.Monad.IOSync.Class (class MonadIOSync, liftIOSync)

-- | Start an asynchronous IO computation. It will be cancelled on cleanup.
startIO :: forall m. MonadIOSync m => MonadCleanup m => IO Unit -> m Unit
startIO action = do
  fiber <- liftIOSync $ liftEff $ launchAff $ runIO action
  onCleanup $ liftEff $ launchAff_ $ killFiber (error "Cancelled") fiber
