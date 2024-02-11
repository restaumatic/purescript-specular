module ListSpec where

import Prelude hiding (append)

import BuilderSpec (newDynamic)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (dynText, el_, text)
import Specular.FRP (withDynamic_)
import Specular.FRP.List (dynamicList_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (liftEffect)
import Test.Utils.Dom (runBuilderInDiv)

spec :: Spec Unit
spec = do
  describe "dynamicList_" $ do
    it "builds and updates DOM correctly" $ do
      let states = [ [ 1 ], [], [ 1, 2, 3, 4 ], [ 2, 3, 4 ], [ 2 ] ]

      Tuple dyn fire <- liftEffect $ newDynamic []

      Tuple node1 _ <- runBuilderInDiv
        $ withDynamic_ dyn \array ->
            for_ array $ \item ->
              el_ "p" $ text $ show item

      Tuple node2 _ <- runBuilderInDiv
        $ dynamicList_ dyn
        $ \item ->
            -- | let mapFn = \x -> trace ("update " <> show x) (\_ -> show x)
            let
              mapFn = \x -> show x
            in
              el_ "p" $ dynText $ map mapFn item

      for_ states $ \state -> do
        liftEffect (fire state)
        delay (Milliseconds 1.0)
        html1 <- liftEffect (innerHTML node1)
        html2 <- liftEffect (innerHTML node2)
        html1 `shouldEqual` html2
