module NewBuilderSpec where

import Prelude hiding (append)

import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Element (attr, attrD, el, el_, rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.FRP (newDynamic)
import Test.Spec (Spec, describe, it)
import Test.Utils (liftEffect, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), runBuilderInDiv, runBuilderInDiv')

spec :: Spec Unit
spec = describe "New Builder (Specular.Dom.Element)" do
  it "builds static DOM" $ withLeakCheck do
    Tuple node result <- runBuilderInDiv do
       el "div" [attr ("class" := "content")] do
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
       el_ "div" [attrD dynamic]

    liftEffect (innerHTML node) `shouldReturn`
      """<div k2="v2" k1="v1"></div>"""

    liftEffect $ set $ "k1" := "v1.1" <> "k3" := "v3"

    liftEffect (innerHTML node) `shouldReturn`
      """<div k1="v1.1" k3="v3"></div>"""

    -- clean up
    liftEffect unsub
