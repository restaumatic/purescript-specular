module WeakDynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Specular.FRP (holdDyn, newEvent, subscribeWeakDyn_, uniqWeakDynBy, weaken)
import Specular.FRP.WeakDynamic (subscribeWeakDyn)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue, withLeakCheck)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "WeakDynamic" $ do

  describe "pure" $ do
    it "has a value immediately" $ do
      log <- ioSync $ newRef []
      _ <- ioSync $ runCleanupT $
        subscribeWeakDyn_ (\x -> append log x) $
          pure 0

      log `shouldHaveValue` [0]

  describe "subscribeWeakDyn" $ do
    it "updates the resulting Dynamic" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newRef []
      Tuple dyn _ <- ioSync $ runCleanupT $ map weaken $ holdDyn 1 event

      Tuple derivedDyn _ <- ioSync $ runCleanupT $ subscribeWeakDyn (\x ->
        do
          append log (Left x)
          pure (2 * x)
        ) dyn

      _ <- ioSync $ execCleanupT $ subscribeWeakDyn_ (\x -> append log (Right x)) derivedDyn

      ioSync $ fire 5

      log `shouldHaveValue` [Left 1, Right 2, Left 5, Right 10]

  describe "uniqWeakDynBy" $ do
    it "updates value only when it changes" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newRef []
      Tuple wdyn unsub1 <- ioSync $ runCleanupT do
         dyn <- holdDyn 0 event
         uniqWeakDynBy eq (weaken dyn)

      unsub2 <- ioSync $ execCleanupT $ subscribeWeakDyn_ (\x -> append log x) wdyn
      log `shouldHaveValue` [0]

      clear log
      ioSync $ fire 0
      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 2

      log `shouldHaveValue` [1,2]

      -- clean up
      ioSync unsub1
      ioSync unsub2
