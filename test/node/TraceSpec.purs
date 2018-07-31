module TraceSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Tuple (Tuple(..))
import Specular.FRP (holdDyn, newEvent, subscribeDyn_, subscribeEvent_)
import Specular.FRP.Base (traceDynIO, traceEventIO)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it)
import Test.Utils (append, liftEffect, shouldHaveValue, withLeakCheck)

spec :: Spec Unit
spec = describe "Trace" $ do

  describe "event" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []

      let event' = traceEventIO (append log <<< show) event

      unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (\_ -> pure unit) event'

      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 5

      log `shouldHaveValue` ["1", "2", "5"]

      liftEffect unsub

  describe "dynamic" $ do
    it "some tracing" $ withLeakCheck do
      {event, fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []

      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event
      let dyn' = traceDynIO (append log <<< show) dyn

      unsub <- liftEffect $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn'

      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 5

      log `shouldHaveValue` ["1", "2", "5"]

      liftEffect unsub
      liftEffect unsub1
