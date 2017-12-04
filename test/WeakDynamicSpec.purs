module WeakDynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.IORef (newIORef)
import Data.Tuple (Tuple(..))
import Specular.FRP (foldDyn, holdDyn, newEvent, subscribeDyn_, subscribeWeakDyn_)
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
