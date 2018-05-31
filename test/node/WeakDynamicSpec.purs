module WeakDynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Specular.Internal.Effect (newRef)
import Data.Tuple (Tuple(..))
import Specular.FRP (holdDyn, newEvent, subscribeWeakDyn_, weaken)
import Specular.FRP.WeakDynamic (subscribeWeakDyn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue)

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
