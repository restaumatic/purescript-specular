module EventSpec where

import Prelude hiding (append)
import Control.Monad.Cleanup (execCleanupT)
import Effect.Ref (new)
import Data.Maybe (Maybe(..))
import Specular.FRP (filterMapEvent, holdDyn, leftmost, newBehavior, newEvent, sampleAt, subscribeEvent_)
import Specular.FRP.Base (subscribeDyn_)
import Test.Spec (Spec, describe, it)
import Test.Utils (append, clear, liftEffect, shouldHaveValue, withLeakCheck)

spec :: Spec Unit
spec = describe "Event" $ do

  it "pushes values to subscribers, honors unsubscribe" $ withLeakCheck $ do
    {event,fire} <- liftEffect newEvent
    log <- liftEffect $ new []
    unsub1 <- liftEffect $ execCleanupT $ subscribeEvent_ (\x -> append log $ "1:" <> x) event
    unsub2 <- liftEffect $ execCleanupT $ subscribeEvent_ (\x -> append log $ "2:" <> x) event
    liftEffect $ fire "A"
    liftEffect unsub1
    liftEffect $ fire "B"

    log `shouldHaveValue` ["1:A", "2:A", "2:B"]

    -- clean up
    liftEffect unsub2

{-
  describe "mergeEvents" $ do
    it "different root events" $ withLeakCheck $ do
      root1 <- liftEffect newEvent
      root2 <- liftEffect newEvent
      log <- liftEffect $ new []

      let event = mergeEvents pure pure
                              (\l r -> pure $ "both: " <> l <> ", " <> r)
                              root1.event
                              root2.event
      unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      clear log
      liftEffect $ root1.fire "left"
      log `shouldHaveValue` ["left"]

      clear log
      liftEffect $ root1.fire "right"
      log `shouldHaveValue` ["right"]

      -- clean up
      liftEffect unsub

    it "coincidence" $ withLeakCheck $ do
      root <- liftEffect newEvent
      log <- liftEffect $ new []

      let event = mergeEvents
                    (\x -> pure $ "left: " <> x)
                    (\x -> pure $ "right: " <> x)
                    (\l r -> pure $ "both: " <> l <> ", " <> r)
                    root.event root.event

      unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      liftEffect $ root.fire "root"
      log `shouldHaveValue` ["both: root, root"]

      -- clean up
      liftEffect unsub

-}

  it "sampleAt" $ withLeakCheck $ do
    root <- liftEffect newEvent
    b <- liftEffect $ newBehavior "A"
    log <- liftEffect $ new []

    let event = sampleAt root.event b.behavior
    unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

    liftEffect $ root.fire ("1" <> _)
    liftEffect $ b.set "B"
    liftEffect $ root.fire ("2" <> _)
    log `shouldHaveValue` ["1A", "2B"]

    -- clean up
    liftEffect unsub

  it "filterMapEvent" $ withLeakCheck $ do
    root <- liftEffect newEvent
    log <- liftEffect $ new []

    let event = filterMapEvent (\x -> if x < 5 then Just (2 * x) else Nothing) root.event
    unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

    liftEffect $ root.fire 1
    liftEffect $ root.fire 10
    liftEffect $ root.fire 3
    liftEffect $ root.fire 4
    log `shouldHaveValue` [2, 6, 8]

    -- clean up
    liftEffect unsub

  describe "leftmost" $ do
    it "different root events" $ withLeakCheck $ do
      root1 <- liftEffect newEvent
      root2 <- liftEffect newEvent
      log <- liftEffect $ new []

      let event = leftmost [ root1.event, root2.event ]
      unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      clear log
      liftEffect $ root1.fire "left"
      log `shouldHaveValue` ["left"]

      clear log
      liftEffect $ root1.fire "right"
      log `shouldHaveValue` ["right"]

      -- clean up
      liftEffect unsub

    it "coincidence chooses leftmost" $ withLeakCheck $ do
      root <- liftEffect newEvent
      log <- liftEffect $ new []

      let event = leftmost [ 1 <$ root.event, 2 <$ root.event, 3 <$ root.event ]

      unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      liftEffect $ root.fire unit
      log `shouldHaveValue` [ 1 ]

      -- clean up
      liftEffect unsub

  it "does not occur during frames started by firing listeners" $ withLeakCheck $ do
    root <- liftEffect newEvent
    log <- liftEffect $ new []

    unsub <- liftEffect $ execCleanupT $ subscribeEvent_ (\_ -> do
        unsub2 <- execCleanupT $ do
           dyn <- holdDyn 0 root.event
           subscribeDyn_ (append log) dyn
        unsub2
      ) root.event

    liftEffect $ root.fire 1
    log `shouldHaveValue` [ 0 ]

    -- clean up
    liftEffect unsub

  it "events are delivered in order of firing" $ withLeakCheck $ do
    {event,fire} <- liftEffect newEvent
    log <- liftEffect $ new []

    unsub1 <- liftEffect $ execCleanupT $ flip subscribeEvent_ event \x ->
      when (x == "first") $
        fire "second"

    unsub2 <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

    liftEffect $ fire "first"

    log `shouldHaveValue` ["first", "second"]

    -- clean up
    liftEffect unsub1
    liftEffect unsub2
