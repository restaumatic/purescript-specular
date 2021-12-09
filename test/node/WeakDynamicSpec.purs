module WeakDynamicSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Specular.FRP (holdDyn, newEvent, subscribeWeakDyn_, uniqWeakDynBy, weaken)
import Specular.FRP.WeakDynamic (subscribeWeakDyn)
import Effect.Ref (new)
import Test.Spec (Spec, describe, it)
import Test.Utils (append, clear, liftEffect, shouldHaveValue, withLeakCheck)

spec :: Spec Unit
spec = describe "WeakDynamic" $ do

  describe "pure" $ do
    it "has a value immediately" $ do
      log <- liftEffect $ new []
      _ <- liftEffect $ runCleanupT
        $ subscribeWeakDyn_ (\x -> append log x)
        $
          pure 0

      log `shouldHaveValue` [ 0 ]

  describe "subscribeWeakDyn" $ do
    it "updates the resulting Dynamic" $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple dyn _ <- liftEffect $ runCleanupT $ map weaken $ holdDyn 1 event

      Tuple derivedDyn _ <- liftEffect $ runCleanupT $ subscribeWeakDyn
        ( \x ->
            do
              append log (Left x)
              pure (2 * x)
        )
        dyn

      _ <- liftEffect $ execCleanupT $ subscribeWeakDyn_ (\x -> append log (Right x)) derivedDyn

      liftEffect $ fire 5

      log `shouldHaveValue` [ Left 1, Right 2, Left 5, Right 10 ]

  describe "uniqWeakDynBy" $ do
    it "updates value only when it changes" $ withLeakCheck $ do
      { event, fire } <- liftEffect newEvent
      log <- liftEffect $ new []
      Tuple wdyn unsub1 <- liftEffect $ runCleanupT do
        dyn <- holdDyn 0 event
        uniqWeakDynBy eq (weaken dyn)

      unsub2 <- liftEffect $ execCleanupT $ subscribeWeakDyn_ (\x -> append log x) wdyn
      log `shouldHaveValue` [ 0 ]

      clear log
      liftEffect $ fire 0
      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 2

      log `shouldHaveValue` [ 1, 2 ]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
