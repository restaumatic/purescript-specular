module BrowserMain where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import BuilderSpec as BuilderSpec
import InputWidgetsSpec as InputWidgetsSpec

import Examples.Counter as Counter
import Examples.RegistrationForm as RegistrationForm
import Examples.AsyncRequest as AsyncRequest

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  BuilderSpec.spec
  InputWidgetsSpec.spec

  describe "example apps" $ do
    Counter.spec
    RegistrationForm.spec
    AsyncRequest.spec
