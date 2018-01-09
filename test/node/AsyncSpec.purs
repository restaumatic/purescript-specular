module AsyncSpec where

import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync.Class (liftIOSync)
import Data.IORef (newIORef)
import Prelude hiding (append)
import Specular.FRP (newEvent, subscribeEvent_)
import Specular.FRP.Async (performEvent)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = do
  describe "asyncRequestMaybe" $ do
    pending "makes a request for initial value"
    pending "makes a request when the value changes"
    pending "changes to NotRequested when the value is Nothing"

  describe "performEvent" $ do
    it "runs handler and pushes return value to event" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $ do
        result <- performEvent $ map
          (\x -> liftIOSync (append log ("handler:" <> x)) *> pure x)
          event
        subscribeEvent_ (\x -> append log $ "result:" <> x) result

      ioSync $ fire "A"
      ioSync $ fire "B"

      log `shouldHaveValue` ["handler:A", "result:A", "handler:B", "result:B"]
