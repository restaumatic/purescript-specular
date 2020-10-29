module Test.Main where

import Prelude

import AsyncSpec as AsyncSpec
import DynamicSpec as DynamicSpec
import Effect (Effect)
import Effect.Aff (launchAff_)
import EventSpec as EventSpec
import FixSpec as FixSpec
import RIOSpec as RIOSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner (runSpec)
import TraceSpec as TraceSpec
import WeakDynamicSpec as WeakDynamicSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  FixSpec.spec
  EventSpec.spec
  DynamicSpec.spec
  WeakDynamicSpec.spec
  RIOSpec.spec
  TraceSpec.spec
  AsyncSpec.spec
