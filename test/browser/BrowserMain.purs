module BrowserMain where

import Prelude

import BuilderSpec as BuilderSpec
import Control.Monad.Eff (Eff)
import Examples.Counter as Counter
import Examples.RegistrationForm as RegistrationForm
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  BuilderSpec.spec

  describe "example apps" $ do
    Counter.spec
    RegistrationForm.spec
