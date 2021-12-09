module Control.Monad.Replace where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Effect (Effect)
import Control.Monad.Reader (ReaderT(..), runReaderT)

data Slot m = Slot
  (forall a. m a -> Effect a) -- ^ run inner widget, replace contents
  (Effect Unit) -- ^ destroy
  (Effect (Slot m)) -- ^ Create a new slot after this one

replaceSlot :: forall m a. Slot m -> m a -> Effect a
replaceSlot (Slot replace _ _) = replace

destroySlot :: forall m. Slot m -> Effect Unit
destroySlot (Slot _ destroy _) = destroy

appendSlot :: forall m. Slot m -> Effect (Slot m)
appendSlot (Slot _ _ append) = append

class (Monad m, MonadCleanup m) <= MonadReplace m where
  newSlot :: m (Slot m)

instance monadReplaceReaderT :: MonadReplace m => MonadReplace (ReaderT r m) where
  newSlot = ReaderT $ \env -> slotWith env <$> newSlot
    where
    slotWith :: r -> Slot m -> Slot (ReaderT r m)
    slotWith env slot = Slot
      (\m -> replaceSlot slot (runReaderT m env))
      (destroySlot slot)
      (map (slotWith env) (appendSlot slot))
