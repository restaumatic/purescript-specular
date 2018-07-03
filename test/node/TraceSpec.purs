module TraceSpec where

import Prelude

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Tuple (Tuple(..))
import Debug.Specular (traceDyn, traceEvent)
import Specular.FRP (holdDyn, newEvent, subscribeDyn_, subscribeEvent_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (ioSync, withLeakCheck)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Trace" $ do

  describe "event" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- ioSync newEvent
      let event' = traceEvent "Event" event

      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (\_ -> pure unit) event'

      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 5

      ioSync unsub

  describe "dynamic" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- ioSync newEvent
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn 0 event
      let dyn' = traceDyn "Dynamic" dyn

      unsub <- ioSync $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn'

      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 5

      ioSync unsub
      ioSync unsub1
