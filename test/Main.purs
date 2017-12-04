module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import UniqueMapMutableSpec as UniqueMapMutableSpec
import EventSpec as EventSpec
import DynamicSpec as DynamicSpec
import FixSpec as FixSpec
import WeakDynamicSpec as WeakDynamicSpec

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  FixSpec.spec
  DynamicSpec.spec
  EventSpec.spec
  UniqueMapMutableSpec.spec
  WeakDynamicSpec.spec
