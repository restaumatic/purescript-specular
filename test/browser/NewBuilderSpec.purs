module NewBuilderSpec where

import Prelude hiding (append)

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (ClassName, attrs, attrsD, classWhenD, classesD, dynText, el, el_, rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (emptyWidget, runMainWidgetInBody)
import Specular.FRP (newDynamic)
import Test.Spec (Spec, after_, describe, it, describeOnly)
import Test.Utils (liftEffect, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), runBuilderInDiv, runBuilderInDiv', numChildNodes)
import Specular.Dom.Node.Class (createElement)
import Specular.Dom.Widget (runWidgetInNode)

spec :: Spec Unit
spec =
  after_ (liftEffect clearDocument) $
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

    it "builds static DOM" $ withLeakCheck do
      Tuple node result <- runBuilderInDiv do
         el "div" [attrs ("class" := "content")] do
           text "foo"
           el "span" [] $ text "bar"
           rawHtml """<p class="foo">Raw</p>"""
           text "baz"
         el_ "hr" emptyWidget
         el_ "span" (text "foo")

      liftEffect (innerHTML node) `shouldReturn`
        """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><hr><span>foo</span>"""

    it "updates attributes" $ withLeakCheck do
      {dynamic,set} <- liftEffect $ newDynamic $ "k1" := "v1" <> "k2" := "v2"
      T3 node result unsub <- runBuilderInDiv' do
         el "div" [attrsD dynamic] emptyWidget

      liftEffect (innerHTML node) `shouldReturn`
        """<div k2="v2" k1="v1"></div>"""

      liftEffect $ set $ "k1" := "v1.1" <> "k3" := "v3"

      liftEffect (innerHTML node) `shouldReturn`
        """<div k1="v1.1" k3="v3"></div>"""

      -- clean up
      liftEffect unsub

    describe "classesD" do
      it "single instance" do
        {dynamic: d, set} <- newDynamic ["foo", "bar"]
        liftEffect $ runMainWidgetInBody $ el "div" [attrs ("id":="test"), classesD d] emptyWidget
        liftEffect (getElementClasses "#test") `shouldReturn` ["foo", "bar"]
        liftEffect $ set ["bar", "baz"]
        liftEffect (getElementClasses "#test") `shouldReturn` ["bar", "baz"]
        liftEffect $ set ["bar", "baz"]
        liftEffect (getElementClasses "#test") `shouldReturn` ["bar", "baz"]
        liftEffect $ set []
        liftEffect (getElementClasses "#test") `shouldReturn` []

      it "multiple instances" do
        {dynamic: d1, set: set_d1} <- newDynamic ["foo"]
        {dynamic: d2, set: set_d2} <- newDynamic ["bar"]
        liftEffect $ runMainWidgetInBody $ el "div" [attrs ("id":="test"), classesD d1, classesD d2] emptyWidget
        liftEffect (getElementClasses "#test") `shouldReturn` ["foo", "bar"]
        liftEffect $ set_d1 ["foo", "baz"]
        liftEffect (getElementClasses "#test") `shouldReturn` ["foo", "bar", "baz"]

    it "classWhenD" do
      {dynamic: d, set} <- newDynamic true
      liftEffect $ runMainWidgetInBody $ el "div" [attrs ("id":="test"), classWhenD d "foo"] emptyWidget
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]
      liftEffect $ set false
      liftEffect (getElementClasses "#test") `shouldReturn` []
      liftEffect $ set true
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]
      liftEffect $ set true
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]

    it "dynText" $ withLeakCheck do
      {dynamic,set} <- liftEffect $ newDynamic $ "foo"
      T3 node result unsub <- runBuilderInDiv' do
         el "div" [] $ dynText dynamic

      liftEffect (innerHTML node) `shouldReturn`
        """<div>foo</div>"""

      liftEffect $ set $ "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """<div>bar</div>"""

      -- clean up
      liftEffect unsub

foreign import clearDocument :: Effect Unit
foreign import getElementClasses :: String -> Effect (Array ClassName)
