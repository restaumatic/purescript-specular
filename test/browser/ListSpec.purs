module ListSpec where

import Prelude hiding (append)

import BuilderSpec (newDynamic)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (dynText, el, text)
import Specular.FRP (for, weaken)
import Specular.FRP.List (weakDynamicList_)
import Specular.FRP.Replaceable (weakDynamic_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (ioSync)
import Test.Utils.Dom (runBuilderInDiv)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "weakDynamicList_" $ do
  it "builds and updates DOM correctly" $ do
    let states = [[1], [], [1,2,3,4], [2,3,4], [2]]

    Tuple dyn fire <- ioSync $ newDynamic []
    let wdyn = weaken dyn

    Tuple node1 _ <- runBuilderInDiv $
      weakDynamic_ $ for wdyn $ \array ->
        for_ array $ \item ->
          el "p" $ text $ show item

    Tuple node2 _ <- runBuilderInDiv $
      weakDynamicList_ wdyn $ \item ->
        el "p" $ dynText $ map show item

    for_ states $ \state -> do
      ioSync (fire state)
      html1 <- ioSync (innerHTML node1)
      html2 <- ioSync (innerHTML node2)
      html1 `shouldEqual` html2 
