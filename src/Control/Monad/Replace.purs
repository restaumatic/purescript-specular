module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Control.Monad.IOSync (IOSync)
import Control.Monad.Reader (ReaderT(..), runReaderT)

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

instance monadReplaceReaderT :: MonadReplace m => MonadReplace (ReaderT r m) where
  newSlot = ReaderT $ \env -> slotWith env <$> newSlot
    where
      slotWith :: r -> Slot m -> Slot (ReaderT r m)
      slotWith env (Slot slot) = Slot
        { replace: \m -> slot.replace (runReaderT m env)
        , destroy: slot.destroy
        , append: map (slotWith env) slot.append
        }
