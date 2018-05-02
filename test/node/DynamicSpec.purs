module DynamicSpec where

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Data.Either (Either(..))
import Data.IORef (newIORef)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude hiding (append)
import Specular.FRP (foldDyn, holdDyn, holdUniqDynBy, newEvent, subscribeDyn_)
import Specular.FRP.Base (latestJust, subscribeDyn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue, withLeakCheck, withLeakCheck')

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Dynamic" $ do

  describe "holdDyn" $ do
    it "updates value when someone is subscribed to changes" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn 0 event

      unsub <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]

      clear log
      ioSync $ fire 1
      ioSync unsub
      ioSync $ fire 2

      log `shouldHaveValue` [1]

      -- clean up
      ioSync unsub1

    it "updates value when no one is subscribed" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn 0 event

      ioSync $ fire 2

      unsub2 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      log `shouldHaveValue` [2]

      -- clean up
      ioSync unsub1
      ioSync unsub2

  describe "holdUniqDynBy" $ do
    it "updates value only when it changes" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdUniqDynBy eq 0 event

      unsub2 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
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

  describe "foldDyn" $ do
    it "updates value correctly" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ foldDyn add 0 event

      unsub2 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      ioSync $ fire 1
      ioSync $ fire 2
      ioSync $ fire 3

      log `shouldHaveValue` [0,1,3,6]

      -- clean up
      ioSync unsub1
      ioSync unsub2

  describe "Applicative instance" $ do
    it "works with different root Dynamics" $ withLeakCheck $ do
      ev1 <- ioSync newEvent
      ev2 <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple rootDyn1 unsub1 <- ioSync $ runCleanupT $ holdDyn 0 ev1.event
      Tuple rootDyn2 unsub2 <- ioSync $ runCleanupT $ holdDyn 10 ev2.event

      let dyn = Tuple <$> rootDyn1 <*> rootDyn2
      unsub3 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      ioSync $ ev1.fire 1
      log `shouldHaveValue` [Tuple 0 10, Tuple 1 10]

      clear log
      ioSync $ ev2.fire 5
      log `shouldHaveValue` [Tuple 1 5]

      -- clean up
      ioSync unsub1
      ioSync unsub2
      ioSync unsub3

    it "has no glitches when used with the same root Dynamic" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple rootDyn unsub1 <- ioSync $ runCleanupT $ holdDyn 0 event

      let dyn = Tuple <$> rootDyn <*> (map (_ + 10) rootDyn)
      unsub2 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      ioSync $ fire 1

      log `shouldHaveValue` [Tuple 0 10, Tuple 1 11]

      -- clean up
      ioSync unsub1
      ioSync unsub2

  describe "Monad instance" $ do
    it "works" $ withLeakCheck $ do
      ev1 <- ioSync newEvent
      ev2 <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple rootDynInner unsub1 <- ioSync $ runCleanupT $ holdDyn 0 ev1.event
      Tuple rootDynOuter unsub2 <- ioSync $ runCleanupT $ holdDyn rootDynInner ev2.event

      let dyn = join rootDynOuter
      unsub3 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn
      log `shouldHaveValue` [0]
      clear log

      -- inner fires
      ioSync $ ev1.fire 1
      log `shouldHaveValue` [1]
      clear log

      -- outer fires
      ioSync $ ev2.fire (pure 2)
      log `shouldHaveValue` [2]
      clear log

      -- inner fires when outer not pointing to it
      ioSync $ ev1.fire 10
      log `shouldHaveValue` []

      -- outer fires to itself
      ioSync $ ev2.fire (3 <$ rootDynOuter)
      log `shouldHaveValue` [3]
      clear log

      -- outer fires to itself again
      ioSync $ ev2.fire (4 <$ rootDynOuter)
      log `shouldHaveValue` [4]
      clear log

      -- outer fires to inner
      ioSync $ ev2.fire rootDynInner
      log `shouldHaveValue` [10]
      clear log

      -- extra subscription should not mess things up
      unsub4 <- ioSync $ execCleanupT $ subscribeDyn_ (\_ -> pure unit) dyn
      ioSync $ ev1.fire 15
      ioSync $ ev2.fire rootDynInner
      log `shouldHaveValue` [15, 15]

      -- clean up
      ioSync unsub1
      ioSync unsub2
      ioSync unsub3
      ioSync unsub4

    it "triple bind with the same root" $ withLeakCheck $ do
      ev <- ioSync newEvent
      unsub1 <- ioSync $ execCleanupT $ do
        rootDyn <- holdDyn unit ev.event
        let
          dyn = do
            rootDyn
            rootDyn
            rootDyn

        subscribeDyn_ (\_ -> pure unit) dyn

      withLeakCheck' "first fire" $ ioSync $ ev.fire unit
      withLeakCheck' "second fire" $ ioSync $ ev.fire unit

      -- clean up
      ioSync unsub1

  describe "subscribeDyn" $ do
    it "updates the resulting Dynamic" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn 1 event

      Tuple derivedDyn unsub2 <- ioSync $ runCleanupT $ subscribeDyn (\x ->
        do
          append log (Left x)
          pure (2 * x)
        ) dyn

      unsub3 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log (Right x)) derivedDyn

      ioSync $ fire 5

      log `shouldHaveValue` [Left 1, Right 2, Left 5, Right 10]

      -- clean up
      ioSync unsub1
      ioSync unsub2
      ioSync unsub3

  describe "latestJust" $ do
    it "updates value only when it changes to Just" $ withLeakCheck $ do
      {event,fire} <- ioSync newEvent
      log <- ioSync $ newIORef []
      Tuple dyn unsub1 <- ioSync $ runCleanupT $ holdDyn Nothing event >>= latestJust

      unsub2 <- ioSync $ execCleanupT $ subscribeDyn_ (\x -> append log x) dyn

      ioSync $ fire Nothing
      ioSync $ fire (Just 1)
      ioSync $ fire Nothing
      ioSync $ fire (Just 2)

      log `shouldHaveValue` [Nothing, Just 1, Just 2]

      -- clean up
      ioSync unsub1
      ioSync unsub2
