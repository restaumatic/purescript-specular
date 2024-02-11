module Test.Main where

import Prelude

import AsyncSpec as AsyncSpec
import DynamicSpec as DynamicSpec
import Effect (Effect)
import Effect.Aff (launchAff_)
import EventSpec as EventSpec
import RIOSpec as RIOSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import TraceSpec as TraceSpec

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  EventSpec.spec
  DynamicSpec.spec
  RIOSpec.spec
  TraceSpec.spec
  AsyncSpec.spec
