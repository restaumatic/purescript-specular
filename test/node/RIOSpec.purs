module RIOSpec where

import Control.Monad.Aff (Aff)
import Control.Monad.IOSync.Class (liftIOSync)
import Control.Monad.RIO (RIO, local)
import Control.Monad.RIO as RIO
import Control.Monad.Reader.Class (ask)
import Prelude hiding (append)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (assertValues, ioSync, newSpyIO, trigger)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "RIO" $ do

  it "pure" $ do
    pure 2 `shouldReturn` 2

  it "ask" $ do
    ask `shouldReturn` testEnv

  it "map" $ do
    map (add 1) (pure 2) `shouldReturn` 3

  it "liftIOSync" $ do
    spy <- newSpyIO
    liftIOSync (trigger spy "effect" "value") `shouldReturn` "value"
    assertValues spy ["effect"]

  describe "apply" $ do
    it "pure" $ do
      (pure (add 1) <*> pure 2) `shouldReturn` 3

    it "effects" $ do
      spy <- newSpyIO
      (liftIOSync (trigger spy "f" (add 1)) <*> liftIOSync (trigger spy "x" 2))
        `shouldReturn` 3
      assertValues spy ["f", "x"]

  describe "bind" $ do
    it "pure" $ do
      (pure 1 >>= \x -> pure (x + 2)) `shouldReturn` 3

    it "effects" $ do
      spy <- newSpyIO
      (do x <- liftIOSync (trigger spy "first" "x")
          liftIOSync (trigger spy "second" (x <> "y")))
        `shouldReturn` "xy"
      assertValues spy ["first", "second"]

  it "local" $ do
    local (add 1) ask `shouldReturn` (testEnv + 1)

type TestEnv = Int

testEnv :: TestEnv
testEnv = 1

shouldReturn :: forall r t. Show t => Eq t => RIO TestEnv t -> t -> Aff r Unit
shouldReturn action expected = do
  actual <- runRIO action
  actual `shouldEqual` expected

runRIO :: forall r a. RIO TestEnv a -> Aff r a
runRIO = ioSync <<< RIO.runRIO testEnv
