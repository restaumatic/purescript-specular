module WeakDynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Data.IORef (newIORef)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Specular.FRP (foldDyn, holdDyn, newEvent, subscribeDyn_, subscribeWeakDyn_, weaken)
import Specular.FRP.WeakDynamic (subscribeWeakDyn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "WeakDynamic" $ do

  describe "pure" $ do
    it "has a value immediately" $ do
      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $
        subscribeWeakDyn_ (\x -> append log x) $
          pure 0

      log `shouldHaveValue` [0]

  describe "subscribeWeakDyn" $ do
    it "updates the resulting Dynamic" $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn _ <- ioSync $ runCleanupT $ map weaken $ holdDyn 1 event

      Tuple derivedDyn _ <- ioSync $ runCleanupT $ subscribeWeakDyn (\x ->
        do
          append log (Left x)
          pure (2 * x)
        ) dyn

      _ <- ioSync $ execCleanupT $ subscribeWeakDyn_ (\x -> append log (Right x)) derivedDyn

      ioSync $ fire 5

      log `shouldHaveValue` [Left 1, Right 2, Left 5, Right 10]
