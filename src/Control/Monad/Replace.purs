module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Control.Monad.IOSync (IOSync(..))
import Specular.FRP (Dynamic, subscribeDyn_)

class (Monad m, MonadCleanup m) <= MonadReplace m where
  runReplaceable :: forall a. m a -> m { replace :: m a -> IOSync Unit }
