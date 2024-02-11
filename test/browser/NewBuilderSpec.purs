module NewBuilderSpec where

import Prelude hiding (append)

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (createElement, innerHTML, (:=))
import Specular.Dom.Element (ClassName, attrs, classWhenD, class_, classesD, dynText, el, el_, text)
import Specular.Dom.Element as E
import Specular.Dom.Widget (emptyWidget, runMainWidgetInBody, runWidgetInNode)
import Specular.FRP (newDynamic)
import Specular.Ref as Ref
import Test.Spec (Spec, after_, describe, it)
import Test.Utils (liftEffect, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), numChildNodes, runBuilderInDiv')

spec :: Spec Unit
spec =
  after_ (liftEffect clearDocument) do
    describe "New Builder (Specular.Dom.Element)" do
      describe "runWidgetInNode" do
        it "runWidgetInNode should remove DOM content on cleanup" do
          parent <- liftEffect $ createElement "div"
          Tuple _ remove1 <- liftEffect $ runWidgetInNode parent $ el_ "p" $ text "widget1"

          liftEffect remove1
          liftEffect (innerHTML parent) `shouldReturn` ""
          liftEffect (numChildNodes parent) `shouldReturn` 0

        it "should remove DOM content from multiple instances" do
          parent <- liftEffect $ createElement "div"
          Tuple _ remove1 <- liftEffect $ runWidgetInNode parent $ el_ "p" $ text "widget1"
          Tuple _ remove2 <- liftEffect $ runWidgetInNode parent $ el_ "p" $ text "widget2"

          liftEffect (innerHTML parent) `shouldReturn` """<p>widget1</p><p>widget2</p>"""
          liftEffect remove1
          liftEffect (innerHTML parent) `shouldReturn` """<p>widget2</p>"""
          liftEffect remove2
          liftEffect (innerHTML parent) `shouldReturn` ""
          liftEffect (numChildNodes parent) `shouldReturn` 0

      describe "classesD" do
        it "single instance" do
          { dynamic: d, set } <- newDynamic [ "foo", "bar" ]
          liftEffect $ runMainWidgetInBody $ el "div" [ attrs ("id" := "test"), classesD d ] emptyWidget
          liftEffect (getElementClasses "#test") `shouldReturn` [ "foo", "bar" ]
          liftEffect $ set [ "bar", "baz" ]
          liftEffect (getElementClasses "#test") `shouldReturn` [ "bar", "baz" ]
          liftEffect $ set [ "bar", "baz" ]
          liftEffect (getElementClasses "#test") `shouldReturn` [ "bar", "baz" ]
          liftEffect $ set []
          liftEffect (getElementClasses "#test") `shouldReturn` []

        it "multiple instances" do
          { dynamic: d1, set: set_d1 } <- newDynamic [ "foo" ]
          { dynamic: d2, set: _set_d2 } <- newDynamic [ "bar" ]
          liftEffect $ runMainWidgetInBody $ el "div" [ attrs ("id" := "test"), classesD d1, classesD d2 ] emptyWidget
          liftEffect (getElementClasses "#test") `shouldReturn` [ "foo", "bar" ]
          liftEffect $ set_d1 [ "foo", "baz" ]
          liftEffect (getElementClasses "#test") `shouldReturn` [ "foo", "bar", "baz" ]

        it "allows space-separated classes" do
          { dynamic: d, set } <- newDynamic [ "foo bar" ]
          liftEffect $ runMainWidgetInBody $ el "div" [ attrs ("id" := "test"), classesD d ] emptyWidget
          liftEffect (getElementClasses "#test") `shouldReturn` [ "foo", "bar" ]
          liftEffect $ set [ "bar" ]
          liftEffect (getElementClasses "#test") `shouldReturn` [ "bar" ]
          liftEffect $ set [ "bar  baz  " ]
          liftEffect (getElementClasses "#test") `shouldReturn` [ "bar", "baz" ]
          liftEffect $ set []
          liftEffect (getElementClasses "#test") `shouldReturn` []

      it "classWhenD" do
        { dynamic: d, set } <- newDynamic true
        liftEffect $ runMainWidgetInBody $ el "div" [ attrs ("id" := "test"), classWhenD d "foo" ] emptyWidget
        liftEffect (getElementClasses "#test") `shouldReturn` [ "foo" ]
        liftEffect $ set false
        liftEffect (getElementClasses "#test") `shouldReturn` []
        liftEffect $ set true
        liftEffect (getElementClasses "#test") `shouldReturn` [ "foo" ]
        liftEffect $ set true
        liftEffect (getElementClasses "#test") `shouldReturn` [ "foo" ]

      it "dynText" $ withLeakCheck do
        { dynamic, set } <- liftEffect $ newDynamic $ "foo"
        T3 node _result unsub <- runBuilderInDiv' do
          el "div" [] $ dynText dynamic

        liftEffect (innerHTML node) `shouldReturn`
          """<div>foo</div>"""

        liftEffect $ set $ "bar"

        liftEffect (innerHTML node) `shouldReturn`
          """<div>bar</div>"""

        -- clean up
        liftEffect unsub

      describe "class_" do
        it "allows space-separated classes" do
          liftEffect $ runMainWidgetInBody $ el "div" [ attrs ("id" := "test"), class_ "foo bar  baz  " ] emptyWidget
          liftEffect (getElementClasses "#test") `shouldReturn` [ "foo", "bar", "baz" ]

      it "valueD" do
        ref <- Ref.new "hello"
        liftEffect $ runMainWidgetInBody $
          el "input" [ attrs ("id" := "test"), E.valueD (Ref.value ref) ] emptyWidget
        liftEffect (getElementProperty "#test" "value") `shouldReturn` "hello"
        Ref.write ref "world"
        liftEffect (getElementProperty "#test" "value") `shouldReturn` "world"

      it "checkedD" do
        ref <- Ref.new false
        liftEffect $ runMainWidgetInBody $
          el "input" [ attrs ("id" := "test"), E.checkedD (Ref.value ref) ] emptyWidget
        liftEffect (getElementProperty "#test" "checked") `shouldReturn` false
        Ref.write ref true
        liftEffect (getElementProperty "#test" "checked") `shouldReturn` true

      it "indeterminateD" do
        ref <- Ref.new false
        liftEffect $ runMainWidgetInBody $
          el "input" [ attrs ("id" := "test"), E.indeterminateD (Ref.value ref) ] emptyWidget
        liftEffect (getElementProperty "#test" "indeterminate") `shouldReturn` false
        Ref.write ref true
        liftEffect (getElementProperty "#test" "indeterminate") `shouldReturn` true

foreign import clearDocument :: Effect Unit
foreign import getElementClasses :: String -> Effect (Array ClassName)
foreign import getElementProperty :: forall a. String -> String -> Effect a
