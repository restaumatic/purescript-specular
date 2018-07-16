module TraceSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Tuple (Tuple(..))
import Specular.FRP (holdDyn, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.Base (traceDynIO, traceEventIO)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue, withLeakCheck)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Trace" $ do

  describe "event" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- ioSync newEvent
      log <- ioSync $ newRef []

      let event' = traceEventIO (append log <<< show) event

      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (\_ -> pure unit) event'

      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 5

      log `shouldHaveValue` ["1", "2", "5"]

      ioSync unsub

  describe "dynamic" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- ioSync newEvent
      log <- ioSync $ newRef []

      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn 0 event
      let dyn' = traceDynIO (append log <<< show) dyn

      unsub <- ioSync $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn'

      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 5

      log `shouldHaveValue` ["1", "2", "5"]

      ioSync unsub
      ioSync unsub1
