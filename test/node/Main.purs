module Test.Main where

import Prelude

import AsyncSpec as AsyncSpec
import DynamicSpec as DynamicSpec
import Effect (Effect)
import EventSpec as EventSpec
import FixSpec as FixSpec
import RIOSpec as RIOSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import TraceSpec as TraceSpec
import UniqueMapMutableSpec as UniqueMapMutableSpec
import WeakDynamicSpec as WeakDynamicSpec

main :: Effect Unit
main = run [consoleReporter] do
  FixSpec.spec
  EventSpec.spec
  DynamicSpec.spec
  UniqueMapMutableSpec.spec
  WeakDynamicSpec.spec
  RIOSpec.spec
  TraceSpec.spec
  AsyncSpec.spec
