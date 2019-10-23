module NewBuilderSpec where

import Prelude hiding (append)

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (ClassName, attrs, attrsD, classWhenD, classesD, el, el_, rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widget (runMainWidgetInBody)
import Specular.FRP (newDynamic)
import Test.Spec (Spec, after_, describe, it)
import Test.Utils (liftEffect, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), runBuilderInDiv, runBuilderInDiv')

spec :: Spec Unit
spec =
  after_ (liftEffect clearDocument) $
  describe "New Builder (Specular.Dom.Element)" do
    it "builds static DOM" $ withLeakCheck do
      Tuple node result <- runBuilderInDiv do
         el "div" [attrs ("class" := "content")] do
           text "foo"
           el "span" [] $ text "bar"
           rawHtml """<p class="foo">Raw</p>"""
           text "baz"
         el_ "span" []

      liftEffect (innerHTML node) `shouldReturn`
        """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><span></span>"""

    it "updates attributes" $ withLeakCheck do
      {dynamic,set} <- liftEffect $ newDynamic $ "k1" := "v1" <> "k2" := "v2"
      T3 node result unsub <- runBuilderInDiv' do
         el_ "div" [attrsD dynamic]

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
        liftEffect $ runMainWidgetInBody $ el_ "div" [attrs ("id":="test"), classesD d]
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
        liftEffect $ runMainWidgetInBody $ el_ "div" [attrs ("id":="test"), classesD d1, classesD d2]
        liftEffect (getElementClasses "#test") `shouldReturn` ["foo", "bar"]
        liftEffect $ set_d1 ["foo", "baz"]
        liftEffect (getElementClasses "#test") `shouldReturn` ["foo", "bar", "baz"]

    it "classWhenD" do
      {dynamic: d, set} <- newDynamic true
      liftEffect $ runMainWidgetInBody $ el_ "div" [attrs ("id":="test"), classWhenD d "foo"]
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]
      liftEffect $ set false
      liftEffect (getElementClasses "#test") `shouldReturn` []
      liftEffect $ set true
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]
      liftEffect $ set true
      liftEffect (getElementClasses "#test") `shouldReturn` ["foo"]

foreign import clearDocument :: Effect Unit
foreign import getElementClasses :: String -> Effect (Array ClassName)
