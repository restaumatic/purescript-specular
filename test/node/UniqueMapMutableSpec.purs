module UniqueMapMutableSpec where

import Prelude

import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Specular.Internal.UniqueMap.Mutable as UMM
import Test.Spec (Spec, describe, it)
import Test.Utils (liftEffect, shouldReturn)

spec :: Spec Unit
spec = describe "Specular.Internal.UniqueMap.Mutable" $ do

  it "can insert and retrieve elements" $ do
    m <- liftEffect UMM.new
    key1 <- liftEffect $ UMM.insert 1 m
    key2 <- liftEffect $ UMM.insert 2 m

    liftEffect (UMM.lookup key1 m) `shouldReturn` Just 1
    liftEffect (UMM.lookup key2 m) `shouldReturn` Just 2

  it "can delete elements" $ do
    m <- liftEffect UMM.new
    key1 <- liftEffect $ UMM.insert 1 m
    key2 <- liftEffect $ UMM.insert 2 m

    liftEffect $ UMM.delete key1 m

    liftEffect (UMM.lookup key1 m) `shouldReturn` Nothing
    liftEffect (UMM.lookup key2 m) `shouldReturn` Just 2

  it "can obtain values" $ do
    m <- liftEffect UMM.new
    key1 <- liftEffect $ UMM.insert 1 m
    key2 <- liftEffect $ UMM.insert 2 m

    liftEffect (sort <$> UMM.values m) `shouldReturn` [1,2]
