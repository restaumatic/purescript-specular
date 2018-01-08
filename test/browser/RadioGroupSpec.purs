module RadioGroupSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Data.Foreign (toForeign)
import Data.IORef (newIORef)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue, textInput, textInputValue, textInputValueEventOnEnter)
import Specular.Dom.Widgets.RadioGroup (radioGroup)
import Specular.FRP (never, newEvent)
import Specular.FRP.Base (subscribeDyn_, subscribeEvent_)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, clear, ioSync, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (dispatchEvent, dispatchTrivialEvent, querySelector, runBuilderInDiv)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "radioGroup" $ do
  pending' "works" $ do
    log <- ioSync $ newIORef []
    Tuple div _ <- runBuilderInDiv $ do
      dyn <- radioGroup 
        { options: ["foo", "bar"]
        , initialValueIndex: 0
        , render: \_ _ input -> input
        }
      subscribeDyn_ (append log) dyn

    inputFoo <- ioSync $ querySelector "input:nth-child(1)" div
    inputBar <- ioSync $ querySelector "input:nth-child(2)" div

    log `shouldHaveValue` ["foo"]

    clear log
    ioSync $ dispatchTrivialEvent inputBar "click"
    -- FIXME: the click event doesn't trigger the change in PhantomJS

    log `shouldHaveValue` ["bar"]
