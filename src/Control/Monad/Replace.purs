module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup, CleanupT, onCleanup, runCleanupT)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

newtype Slot m = Slot (SlotInternal m)

type SlotInternal m =
  { replace :: forall a. m a -> IOSync a
  , destroy :: IOSync Unit
  , append :: IOSync (Slot m)
  }

unSlot :: forall m. Slot m -> SlotInternal m
unSlot (Slot x) = x

class (Monad m, MonadCleanup m) <= MonadReplace m where
  newSlot :: m (Slot m)
