module UniqueMapMutableSpec where

import Prelude

import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Specular.Internal.UniqueMap.Mutable as UMM
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (ioSync, shouldReturn)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Specular.Internal.UniqueMap.Mutable" $ do

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
