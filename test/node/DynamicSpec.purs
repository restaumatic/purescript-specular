module DynamicSpec where

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Specular.Internal.Effect (newRef)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude hiding (append)
import Specular.FRP (foldDyn, holdDyn, holdUniqDynBy, newEvent, subscribeDyn_)
import Specular.FRP.Base (latestJust, subscribeDyn)
import Test.Spec (Spec, describe, it)
import Test.Utils (append, clear, liftEffect, shouldHaveValue, withLeakCheck, withLeakCheck')

spec :: Spec Unit
spec = describe "Dynamic" $ do

  describe "holdDyn" $ do
    it "updates value when someone is subscribed to changes" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      unsub <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]

      clear log
      liftEffect $ fire 1
      liftEffect unsub
      liftEffect $ fire 2

      log `shouldHaveValue` [1]

      -- clean up
      liftEffect unsub1

    it "updates value when no one is subscribed" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      liftEffect $ fire 2

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      log `shouldHaveValue` [2]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "holdUniqDynBy" $ do
    it "updates value only when it changes" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdUniqDynBy eq 0 event

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]

      clear log
      liftEffect $ fire 0
      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 2

      log `shouldHaveValue` [1,2]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "foldDyn" $ do
    it "updates value correctly" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ foldDyn add 0 event

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire 1
      liftEffect $ fire 2
      liftEffect $ fire 3

      log `shouldHaveValue` [0,1,3,6]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "Applicative instance" $ do
    it "works with different root Dynamics" $ withLeakCheck $ do
      ev1 <- liftEffect newEvent
      ev2 <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple rootDyn1 unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 ev1.event
      Tuple rootDyn2 unsub2 <- liftEffect $ runCleanupT $ holdDyn 10 ev2.event

      let dyn = Tuple <$> rootDyn1 <*> rootDyn2
      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ ev1.fire 1
      log `shouldHaveValue` [Tuple 0 10, Tuple 1 10]

      clear log
      liftEffect $ ev2.fire 5
      log `shouldHaveValue` [Tuple 1 5]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
      liftEffect unsub3

    it "has no glitches when used with the same root Dynamic" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple rootDyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 event

      let dyn = Tuple <$> rootDyn <*> (map (_ + 10) rootDyn)
      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire 1

      log `shouldHaveValue` [Tuple 0 10, Tuple 1 11]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "Monad instance" $ do
    it "works" $ withLeakCheck $ do
      ev1 <- liftEffect newEvent
      ev2 <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple rootDynInner unsub1 <- liftEffect $ runCleanupT $ holdDyn 0 ev1.event
      Tuple rootDynOuter unsub2 <- liftEffect $ runCleanupT $ holdDyn rootDynInner ev2.event

      let dyn = join rootDynOuter
      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]
      clear log

      -- inner fires
      liftEffect $ ev1.fire 1
      log `shouldHaveValue` [1]
      clear log

      -- outer fires
      liftEffect $ ev2.fire (pure 2)
      log `shouldHaveValue` [2]
      clear log

      -- inner fires when outer not pointing to it
      liftEffect $ ev1.fire 10
      log `shouldHaveValue` []

      -- outer fires to itself
      liftEffect $ ev2.fire (3 <$ rootDynOuter)
      log `shouldHaveValue` [3]
      clear log

      -- outer fires to itself again
      liftEffect $ ev2.fire (4 <$ rootDynOuter)
      log `shouldHaveValue` [4]
      clear log

      -- outer fires to inner
      liftEffect $ ev2.fire rootDynInner
      log `shouldHaveValue` [10]
      clear log

      -- extra subscription should not mess things up
      unsub4 <- liftEffect $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn
      liftEffect $ ev1.fire 15
      liftEffect $ ev2.fire rootDynInner
      log `shouldHaveValue` [15, 15]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
      liftEffect unsub3
      liftEffect unsub4

    it "triple bind with the same root" $ withLeakCheck $ do
      ev <- liftEffect newEvent
      unsub1 <- liftEffect $ execCleanupT $ do
        rootDyn <- holdDyn unit ev.event
        let
          dyn = do
            rootDyn
            rootDyn
            rootDyn

        subscribeDyn_ (\_ -> pure unit) dyn

      withLeakCheck' "first fire" $ liftEffect $ ev.fire unit
      withLeakCheck' "second fire" $ liftEffect $ ev.fire unit

      -- clean up
      liftEffect unsub1

  describe "subscribeDyn" $ do
    it "updates the resulting Dynamic" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn 1 event

      Tuple derivedDyn unsub2 <- liftEffect $ runCleanupT $ subscribeDyn (\x ->
        do
          append log (Left x)
          pure (2 * x)
        ) dyn

      unsub3 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log (Right x)) derivedDyn

      liftEffect $ fire 5

      log `shouldHaveValue` [Left 1, Right 2, Left 5, Right 10]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
      liftEffect unsub3

  describe "latestJust" $ do
    it "updates value only when it changes to Just" $ withLeakCheck $ do
      {event,fire} <- liftEffect newEvent
      log <- liftEffect $ newRef []
      Tuple dyn unsub1 <- liftEffect $ runCleanupT $ holdDyn Nothing event >>= latestJust

      unsub2 <- liftEffect $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      liftEffect $ fire Nothing
      liftEffect $ fire (Just 1)
      liftEffect $ fire Nothing
      liftEffect $ fire (Just 2)

      log `shouldHaveValue` [Nothing, Just 1, Just 2]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2
