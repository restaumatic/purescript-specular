module DynamicSpec where

import Prelude hiding (append)

import Data.IORef (newIORef)
import Data.Maybe (Maybe(..))
import Specular.Frame (filterMapEvent, holdDyn, mergeEvents, newBehavior, newEvent, sampleAt, subscribeDyn_, subscribeEvent_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Dynamic" $ do

  describe "holdDyn" $ do
    it "works when someone is subscribed to changes" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      dyn <- ioSync $ holdDyn 0 event

      unsub <- ioSync $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]

      clear log
      ioSync $ fire 1
      ioSync unsub
      ioSync $ fire 2

      log `shouldHaveValue` [1]

    it "works when no one is subscribed" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      dyn <- ioSync $ holdDyn 0 event

      ioSync $ fire 2

      unsub <- ioSync $ subscribeDyn_ (\x -> append log x) dyn

      log `shouldHaveValue` [2]
