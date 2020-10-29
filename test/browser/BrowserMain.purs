module BrowserMain where

import Prelude

import BuilderSpec as BuilderSpec
import Effect (Effect)
import Effect.Aff (launchAff_)
import Examples.AsyncRequest as AsyncRequest
import Examples.Counter as Counter
import Examples.RegistrationForm as RegistrationForm
import InputWidgetsSpec as InputWidgetsSpec
import ListSpec as ListSpec
import NewBuilderSpec as NewBuilderSpec
import RadioGroupSpec as RadioGroupSpec
import Test.Spec (describe, mapSpecTree)
import Data.Newtype (unwrap)
import Test.Spec.Mocha (runMocha)

main :: Effect Unit
main = runMocha $ mapSpecTree (pure <<< unwrap) identity do
  BuilderSpec.spec
  NewBuilderSpec.spec
  InputWidgetsSpec.spec
  ListSpec.spec
  RadioGroupSpec.spec

  describe "example apps" do
    Counter.spec
    RegistrationForm.spec
    AsyncRequest.spec
