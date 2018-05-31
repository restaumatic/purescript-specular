module RadioGroupSpec where

import Prelude hiding (append)

import Specular.Internal.Effect (newRef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Widgets.RadioGroup (radioGroup)
import Specular.FRP.Base (subscribeDyn_)
import Test.Spec (Spec, describe, pending')
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue)
import Test.Utils.Dom (dispatchTrivialEvent, querySelector, runBuilderInDiv)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "radioGroup" $ do
  pending' "works" $ do
    log <- ioSync $ newRef []
    Tuple div _ <- runBuilderInDiv $ do
      dyn <- radioGroup 
        { options: ["foo", "bar"]
        , initialValueIndex: 0
        , render: \_ _ input -> input (pure mempty)
        }
      subscribeDyn_ (append log) dyn

    inputFoo <- ioSync $ querySelector "input:nth-child(1)" div
    inputBar <- ioSync $ querySelector "input:nth-child(2)" div

    log `shouldHaveValue` ["foo"]

    clear log
    ioSync $ dispatchTrivialEvent inputBar "click"
    -- FIXME: the click event doesn't trigger the change in PhantomJS

    log `shouldHaveValue` ["bar"]
