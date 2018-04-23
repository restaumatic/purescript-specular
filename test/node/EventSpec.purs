module EventSpec where

import Control.Monad.Cleanup (execCleanupT)
import Data.IORef (newIORef)
import Data.Maybe (Maybe(..))
import Prelude hiding (append)
import Specular.FRP (filterMapEvent, holdDyn, leftmost, mergeEvents, newBehavior, newEvent, sampleAt, subscribeEvent_)
import Specular.FRP.Base (subscribeDyn_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue, withLeakCheck)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Event" $ do

  it "pushes values to subscribers, honors unsubscribe" $ withLeakCheck $ do
    {event,fire} <- ioSync newEvent
    log <- ioSync $ newIORef []
    unsub1 <- ioSync $ execCleanupT $ subscribeEvent_ (\x -> append log $ "1:" <> x) event
    unsub2 <- ioSync $ execCleanupT $ subscribeEvent_ (\x -> append log $ "2:" <> x) event
    ioSync $ fire "A"
    ioSync unsub1
    ioSync $ fire "B"

    log `shouldHaveValue` ["1:A", "2:A", "2:B"]

    -- clean up
    ioSync unsub2

  describe "mergeEvents" $ do
    it "different root events" $ withLeakCheck $ do
      root1 <- ioSync newEvent
      root2 <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = mergeEvents pure pure
                              (\l r -> pure $ "both: " <> l <> ", " <> r)
                              root1.event
                              root2.event
      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      clear log
      ioSync $ root1.fire "left"
      log `shouldHaveValue` ["left"]

      clear log
      ioSync $ root1.fire "right"
      log `shouldHaveValue` ["right"]

      -- clean up
      ioSync unsub

    it "coincidence" $ withLeakCheck $ do
      root <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = mergeEvents
                    (\x -> pure $ "left: " <> x)
                    (\x -> pure $ "right: " <> x)
                    (\l r -> pure $ "both: " <> l <> ", " <> r)
                    root.event root.event

      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      ioSync $ root.fire "root"
      log `shouldHaveValue` ["both: root, root"]

      -- clean up
      ioSync unsub

  it "sampleAt" $ withLeakCheck $ do
    root <- ioSync newEvent
    b <- ioSync $ newBehavior "A"
    log <- ioSync $ newIORef []

    let event = sampleAt root.event b.behavior
    unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

    ioSync $ root.fire ("1" <> _)
    ioSync $ b.set "B"
    ioSync $ root.fire ("2" <> _)
    log `shouldHaveValue` ["1A", "2B"]

    -- clean up
    ioSync unsub

  it "filterMapEvent" $ withLeakCheck $ do
    root <- ioSync newEvent
    log <- ioSync $ newIORef []

    let event = filterMapEvent (\x -> if x < 5 then Just (2 * x) else Nothing) root.event
    unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

    ioSync $ root.fire 1
    ioSync $ root.fire 10
    ioSync $ root.fire 3
    ioSync $ root.fire 4
    log `shouldHaveValue` [2, 6, 8]

    -- clean up
    ioSync unsub

  describe "leftmost" $ do
    it "different root events" $ withLeakCheck $ do
      root1 <- ioSync newEvent
      root2 <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = leftmost [ root1.event, root2.event ]
      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      clear log
      ioSync $ root1.fire "left"
      log `shouldHaveValue` ["left"]

      clear log
      ioSync $ root1.fire "right"
      log `shouldHaveValue` ["right"]

      -- clean up
      ioSync unsub

    it "coincidence chooses leftmost" $ withLeakCheck $ do
      root <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = leftmost [ 1 <$ root.event, 2 <$ root.event, 3 <$ root.event ]

      unsub <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      ioSync $ root.fire unit
      log `shouldHaveValue` [ 1 ]

      -- clean up
      ioSync unsub

  it "does not occur during frames started by firing listeners" $ withLeakCheck $ do
    root <- ioSync newEvent
    log <- ioSync $ newIORef []

    unsub <- ioSync $ execCleanupT $ subscribeEvent_ (\_ -> do
        unsub2 <- execCleanupT $ do
           dyn <- holdDyn 0 root.event
           subscribeDyn_ (append log) dyn
        unsub2
      ) root.event

    ioSync $ root.fire 1
    log `shouldHaveValue` [ 0 ]

    -- clean up
    ioSync unsub
