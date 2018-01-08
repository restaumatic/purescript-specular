module BrowserMain where

import Prelude

import BuilderSpec as BuilderSpec
import Control.Monad.Eff (Eff)
import Examples.AsyncRequest as AsyncRequest
import Examples.Counter as Counter
import Examples.RegistrationForm as RegistrationForm
import InputWidgetsSpec as InputWidgetsSpec
import ListSpec as ListSpec
import RadioGroupSpec as RadioGroupSpec
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  BuilderSpec.spec
  InputWidgetsSpec.spec
  ListSpec.spec
  RadioGroupSpec.spec

  describe "example apps" $ do
    Counter.spec
    RegistrationForm.spec
    AsyncRequest.spec
