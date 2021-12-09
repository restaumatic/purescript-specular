module RIOSpec where

import Prelude hiding (append)

import Control.Monad.Reader.Class (ask)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Specular.Internal.RIO (RIO, local)
import Specular.Internal.RIO as RIO
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (assertValues, newSpyIO, trigger)

spec :: Spec Unit
spec = describe "RIO" $ do

  it "pure" $ do
    pure 2 `shouldReturn` 2

  it "ask" $ do
    ask `shouldReturn` testEnv

  it "map" $ do
    map (add 1) (pure 2) `shouldReturn` 3

  it "liftEffect" $ do
    spy <- newSpyIO
    liftEffect (trigger spy "effect" "value") `shouldReturn` "value"
    assertValues spy [ "effect" ]

  describe "apply" $ do
    it "pure" $ do
      (pure (add 1) <*> pure 2) `shouldReturn` 3

    it "effects" $ do
      spy <- newSpyIO
      (liftEffect (trigger spy "f" (add 1)) <*> liftEffect (trigger spy "x" 2))
        `shouldReturn` 3
      assertValues spy [ "f", "x" ]

  describe "bind" $ do
    it "pure" $ do
      (pure 1 >>= \x -> pure (x + 2)) `shouldReturn` 3

    it "effects" $ do
      spy <- newSpyIO
      ( do
          x <- liftEffect (trigger spy "first" "x")
          liftEffect (trigger spy "second" (x <> "y"))
      )
        `shouldReturn` "xy"
      assertValues spy [ "first", "second" ]

  it "local" $ do
    local (add 1) ask `shouldReturn` (testEnv + 1)

type TestEnv = Int

testEnv :: TestEnv
testEnv = 1

shouldReturn :: forall t. Show t => Eq t => RIO TestEnv t -> t -> Aff Unit
shouldReturn action expected = do
  actual <- runRIO action
  actual `shouldEqual` expected

runRIO :: forall a. RIO TestEnv a -> Aff a
runRIO = liftEffect <<< RIO.runRIO testEnv
