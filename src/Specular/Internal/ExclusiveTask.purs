module Specular.Internal.ExclusiveTask where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchJust)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, error, joinFiber, killFiber, launchAff_, launchSuspendedAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as ERef
import Unsafe.Reference (unsafeRefEq)

data State
  = Idle
  | Running (Fiber Unit)

newtype ExclusiveTask = ExclusiveTask
  { state :: ERef.Ref State
  }

new :: forall m. MonadEffect m => m ExclusiveTask
new = do
  state_ <- liftEffect $ ERef.new Idle
  pure $ ExclusiveTask { state: state_ }

cancel :: forall m. MonadEffect m => ExclusiveTask -> m Unit
cancel (ExclusiveTask self) = liftEffect do
  state <- ERef.read self.state
  case state of
    Idle ->
      pure unit
    Running fiber ->
      launchAff_ do
        killFiber (error "Cancelled") fiber
        -- TODO: any race conditions here?
        liftEffect $ ERef.write Idle self.state

-- | Run an Aff action in this exclusive task slot.
-- | If there was a previous task running, it is first cancelled.
run :: ExclusiveTask -> Aff Unit -> Effect Unit
run (ExclusiveTask self) block = do
  newFiber <- launchSuspendedAff block
  launchAff_ $ catchCancelled do
    state <- liftEffect $ ERef.read self.state
    case state of
      Idle ->
        pure unit
      Running fiber ->
        killFiber cancelledError fiber

    liftEffect $ ERef.write (Running newFiber) self.state
    -- Only now resume the new fiber
    joinFiber newFiber

    liftEffect $ ERef.write Idle self.state

cancelledError :: Error
cancelledError = error "Cancelled"

isCancelledError :: Error -> Boolean
isCancelledError e = e `unsafeRefEq` cancelledError

catchCancelled :: forall m. MonadError Error m => m Unit -> m Unit
catchCancelled block =
  catchJust (\e -> if isCancelledError e then Just e else Nothing) block (\_ -> pure unit)
