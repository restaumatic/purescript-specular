module ListSpec where

import Prelude hiding (append)

import Effect.Aff (Aff, Milliseconds(..), delay)
import BuilderSpec (newDynamic)
import Data.Foldable (for_, traverse_)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.FRP (changedW, dynamic_, subscribeEvent_, weaken)
import Specular.FRP.List (weakDynamicList, weakDynamicList_, dynamicList_)
import Specular.FRP.Replaceable (weakDynamic_)
import Specular.FRP.WeakDynamic (attachWeakDynWith)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (append, liftEffect, shouldHaveValue, yieldAff)
import Test.Utils.Dom (runBuilderInDiv)
import Debug.Trace

spec :: Spec Unit
spec = do
  describe "weakDynamicList_" $ do
    pending' "builds and updates DOM correctly" $ do
      let states = [[1], [], [1,2,3,4], [2,3,4], [2]]

      Tuple dyn fire <- liftEffect $ newDynamic []
      let wdyn = weaken dyn

      Tuple node1 _ <- runBuilderInDiv $
        weakDynamic_ $ wdyn <#> \array ->
          for_ array $ \item ->
            el "p" $ text $ show item

      Tuple node2 _ <- runBuilderInDiv $
        weakDynamicList_ wdyn $ \item ->
          el "p" $ dynText $ map show item

      for_ states $ \state -> do
        liftEffect (fire state)
        yieldAff
        html1 <- liftEffect (innerHTML node1)
        html2 <- liftEffect (innerHTML node2)
        html1 `shouldEqual` html2 

  describe "dynamicList_" $ do
    it "builds and updates DOM correctly" $ do
      let states = [[1], [], [1,2,3,4], [2,3,4], [2]]
      let states = [[1], [2]]

      Tuple dyn fire <- liftEffect $ newDynamic []

      Tuple node1 _ <- runBuilderInDiv $
        dynamic_ $ dyn <#> \array ->
          for_ array $ \item ->
            el "p" $ text $ show item

      Tuple node2 _ <- runBuilderInDiv $
        dynamicList_ dyn $ \item ->
          el "p" $ dynText $ weaken $ map (\x -> trace ("update " <> show x) (\_ -> show x)) item

      for_ states $ \state -> do
        liftEffect (fire state)
        delay (Milliseconds 1.0)
        html1 <- liftEffect (innerHTML node1)
        html2 <- liftEffect (innerHTML node2)
        html1 `shouldEqual` html2 

  describe "weakDynamicList" $ do
    pending' "updates return value if input changes" $ do
      let states = [[1], [], [1,2,3,4], [2,3,4], [2]]

      Tuple dyn fire <- liftEffect $ newDynamic []
      let wdyn = weaken dyn

      log <- liftEffect $ newRef []

      _ <- runBuilderInDiv do
        resultDyn <- weakDynamicList wdyn pure
        subscribeEvent_ (append log) $
          attachWeakDynWith const (join $ map sequence resultDyn) $
            -- ^ Workaround for glitches in the WeakDynamics given to
            -- individual array elements
          changedW resultDyn

      for_ states \state -> do
        liftEffect $ fire state

      log `shouldHaveValue` states
