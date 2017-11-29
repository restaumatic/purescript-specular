module BrowserMain where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import BuilderSpec as BuilderSpec

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  BuilderSpec.spec
