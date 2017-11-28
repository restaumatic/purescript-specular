module UniqueMapMutableSpec where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.IOSync (IOSync, runIOSync)
import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Data.UniqueMap.Mutable as UMM
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Data.UniqueMap.Mutable" $ do

  it "can insert and retrieve elements" $ do
    m <- ioSync UMM.new
    key1 <- ioSync $ UMM.insert 1 m
    key2 <- ioSync $ UMM.insert 2 m

    ioSync (UMM.lookup key1 m) `shouldReturn` Just 1
    ioSync (UMM.lookup key2 m) `shouldReturn` Just 2

  it "can delete elements" $ do
    m <- ioSync UMM.new
    key1 <- ioSync $ UMM.insert 1 m
    key2 <- ioSync $ UMM.insert 2 m

    ioSync $ UMM.delete key1 m

    ioSync (UMM.lookup key1 m) `shouldReturn` Nothing
    ioSync (UMM.lookup key2 m) `shouldReturn` Just 2

  it "can obtain values" $ do
    m <- ioSync UMM.new
    key1 <- ioSync $ UMM.insert 1 m
    key2 <- ioSync $ UMM.insert 2 m

    ioSync (sort <$> UMM.values m) `shouldReturn` [1,2]

shouldReturn :: forall r t. Show t => Eq t => Aff r t -> t -> Aff r Unit
shouldReturn action expected = do
  actual <- action
  actual `shouldEqual` expected

ioSync :: forall r a. IOSync a -> Aff r a
ioSync = liftEff <<< unsafeCoerceEff <<< runIOSync
