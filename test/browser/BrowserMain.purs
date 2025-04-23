module BrowserMain where

import Prelude

import BuilderSpec as BuilderSpec
import Effect (Effect)
import Examples.AsyncRequest as AsyncRequest
import Examples.CounterRef as CounterRef
import Examples.RegistrationForm as RegistrationForm
import ListSpec as ListSpec
import Test.Spec (describe, mapSpecTree)
import Data.Newtype (unwrap)
import Test.Spec.Mocha (runMocha)

main :: Effect Unit
main = runMocha $ mapSpecTree (pure <<< unwrap) identity do
  BuilderSpec.spec
  ListSpec.spec

  describe "example apps" do
    CounterRef.spec
    RegistrationForm.spec
    AsyncRequest.spec
