module EventSpec where

import Prelude hiding (append)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (snoc)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (Maybe(..))
import Specular.Frame (filterMapEvent, mergeEvents, newBehavior, newEvent, sampleAt, subscribeEvent_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Event" $ do

  it "pushes values to subscribers, honors unsubscribe" $ do
    {event,fire} <- ioSync newEvent
    log <- ioSync $ newIORef []
    unsub1 <- ioSync $ subscribeEvent_ (\x -> append log $ "1:" <> x) event
    unsub2 <- ioSync $ subscribeEvent_ (\x -> append log $ "2:" <> x) event
    ioSync $ fire "A"
    ioSync unsub1
    ioSync $ fire "B"

    log `shouldHaveValue` ["1:A", "2:A", "2:B"]

  describe "mergeEvents" $ do
    it "different root events" $ do
      root1 <- ioSync newEvent
      root2 <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = mergeEvents pure pure
                              (\l r -> pure $ "both: " <> l <> ", " <> r)
                              root1.event
                              root2.event
      _ <- ioSync $ subscribeEvent_ (append log) event

      clear log
      ioSync $ root1.fire "left"
      log `shouldHaveValue` ["left"]

      clear log
      ioSync $ root1.fire "right"
      log `shouldHaveValue` ["right"]

    it "coincidence" $ do
      root <- ioSync newEvent
      log <- ioSync $ newIORef []

      let event = mergeEvents
                    (\x -> pure $ "left: " <> x)
                    (\x -> pure $ "right: " <> x)
                    (\l r -> pure $ "both: " <> l <> ", " <> r)
                    root.event root.event

      _ <- ioSync $ subscribeEvent_ (append log) event

      ioSync $ root.fire "root"
      log `shouldHaveValue` ["both: root, root"]

  it "sampleAt" $ do
    root <- ioSync newEvent
    b <- ioSync $ newBehavior "A"
    log <- ioSync $ newIORef []

    let event = sampleAt root.event b.behavior
    _ <- ioSync $ subscribeEvent_ (append log) event

    ioSync $ root.fire ("1" <> _)
    ioSync $ b.set "B"
    ioSync $ root.fire ("2" <> _)
    log `shouldHaveValue` ["1A", "2B"]

  it "filterMapEvent" $ do
    root <- ioSync newEvent
    log <- ioSync $ newIORef []

    let event = filterMapEvent (\x -> if x < 5 then Just (2 * x) else Nothing) root.event
    _ <- ioSync $ subscribeEvent_ (append log) event

    ioSync $ root.fire 1
    ioSync $ root.fire 10
    ioSync $ root.fire 3
    ioSync $ root.fire 4
    log `shouldHaveValue` [2, 6, 8]


append :: forall a. IORef (Array a) -> a -> IOSync Unit
append ref value = modifyIORef ref (\a -> snoc a value)

clear :: forall a r. IORef (Array a) -> Aff r Unit
clear ref = ioSync $ writeIORef ref []

shouldHaveValue :: forall a r. Eq a => Show a => IORef a -> a -> Aff r Unit
shouldHaveValue ref expected = ioSync (readIORef ref) `shouldReturn` expected

shouldReturn :: forall r t. Show t => Eq t => Aff r t -> t -> Aff r Unit
shouldReturn action expected = do
  actual <- action
  actual `shouldEqual` expected

ioSync :: forall r a. IOSync a -> Aff r a
ioSync = liftEff <<< unsafeCoerceEff <<< runIOSync
