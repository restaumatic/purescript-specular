module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (detach, domEventWithSample, el, elAttr, elDynAttr, elDynAttr', elDynAttrNS', rawHtml, text)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, WeakDynamic, dynamic_, never, subscribeEvent_, switch, weaken)
import Specular.FRP as FRP
import Specular.FRP.Replaceable (dynamic, weakDynamic)
import Specular.FRP.WeakDynamic (switchWeakDyn)
import Specular.Internal.Effect (newRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), dispatchTrivialEvent, querySelector, runBuilderInDiv, runBuilderInDiv')
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec = describe "Builder" $ do
  it "builds static DOM" $ withLeakCheck $ do
    Tuple node result <- runBuilderInDiv $ do
       elAttr "div" ("class" := "content") $ do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         rawHtml """<p class="foo">Raw</p>"""
         text "baz"
       elAttr "span" mempty $ pure unit

    liftEffect (innerHTML node) `shouldReturn`
      """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><span></span>"""

  it "updates attributes" $ withLeakCheck $ do
    Tuple dyn updateDyn <- liftEffect $ newDynamic $ "k1" := "v1" <> "k2" := "v2"
    T3 node result unsub <- runBuilderInDiv' $ do
       elDynAttr "div" (weaken dyn) $ pure unit

    liftEffect (innerHTML node) `shouldReturn`
      """<div k2="v2" k1="v1"></div>"""

    liftEffect $ updateDyn $ "k1" := "v1.1" <> "k3" := "v3"

    liftEffect (innerHTML node) `shouldReturn`
      """<div k1="v1.1" k3="v3"></div>"""

    -- clean up
    liftEffect unsub

  describe "dynamic_" $ do
    it "simple" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """bar"""

      -- clean up
      liftEffect unsub

    it "surrounded by other elements" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         elDynAttr "span" (pure mempty) $ pure unit
         dynamic_ dyn
         elDynAttr "span" (pure mempty) $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>foo<span></span>"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>bar<span></span>"""

      -- clean up
      liftEffect unsub

    it "two subscriptions to the same Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ dyn
         dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foofoo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """barbar"""

      -- clean up
      liftEffect unsub

    it "nested, same Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ $ dyn $> dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """bar"""

      -- clean up
      liftEffect unsub

    it "nested, different Dynamics" $ withLeakCheck $ do
      Tuple dyn1 updateDyn1 <- liftEffect $ newDynamic $ text "foo1"
      Tuple dyn2 updateDyn2 <- liftEffect $ newDynamic $ text "foo2"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ $ map (\x -> x *> dynamic_ dyn2) dyn1

      liftEffect (innerHTML node) `shouldReturn`
        """foo1foo2"""

      liftEffect $ updateDyn1 $ text "bar1"

      liftEffect (innerHTML node) `shouldReturn`
        """bar1foo2"""

      liftEffect $ updateDyn2 $ text "bar2"

      liftEffect (innerHTML node) `shouldReturn`
        """bar1bar2"""

      -- clean up
      liftEffect unsub

    it "with rawHtml" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ rawHtml "<p>raw</p>"
      T3 node result unsub <- runBuilderInDiv' $ do
        el "br" $ pure unit
        dynamic_ dyn
        el "br" $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<br><p>raw</p><br>"""

      liftEffect $ updateDyn $ rawHtml "<input>"

      liftEffect (innerHTML node) `shouldReturn`
        """<br><input><br>"""

      -- clean up
      liftEffect unsub

  describe "domEventWithSample" $ do
    it "dispatches DOM events and handles unsubscribe" $ withLeakCheck $ do
      T3 node {button,event} unsub1 <- runBuilderInDiv' $ do
        Tuple button _ <- elDynAttr' "button" (pure mempty) (text "foo")
        event <- domEventWithSample (\_ -> pure unit) "click" button
        pure {button,event}

      log <- liftEffect $ newRef []
      unsub2 <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      liftEffect $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "integration test - `dynamic`/`weakDynamic` and flattening" $ do
    it "clearCompletedButton from TodoMVC, using Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic false
      let
        anyCompletedTasks :: Dynamic Boolean
        anyCompletedTasks = dyn

      T3 node (result :: Dynamic (Event Unit)) unsub1 <- runBuilderInDiv' $ do
        dynamic $ anyCompletedTasks <#> \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switch result

      log <- liftEffect $ newRef []
      unsub2 <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      liftEffect (innerHTML node) `shouldReturn` ""

      liftEffect $ updateDyn true
      liftEffect (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- liftEffect $ querySelector "button" node
      liftEffect $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

    it "clearCompletedButton from TodoMVC, using WeakDynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic false
      let
        anyCompletedTasks :: WeakDynamic Boolean
        anyCompletedTasks = weaken dyn

      T3 node (result :: WeakDynamic (Event Unit)) unsub1 <- runBuilderInDiv' $ do
        weakDynamic $ anyCompletedTasks <#> \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switchWeakDyn result

      log <- liftEffect $ newRef []
      unsub2 <- liftEffect $ execCleanupT $ subscribeEvent_ (append log) event

      liftEffect (innerHTML node) `shouldReturn` ""

      liftEffect $ updateDyn true
      liftEffect (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- liftEffect $ querySelector "button" node
      liftEffect $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      liftEffect unsub1
      liftEffect unsub2

  describe "detach" $ do
    it "simple case" $ withLeakCheck $ do
      T3 node result unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ text "foo" *> pure 7
         text "bar"
         widget
         pure value

      result `shouldEqual` 7

      liftEffect (innerHTML node) `shouldReturn`
        """barfoo"""

      -- clean up
      liftEffect unsub

    it "double use" $ withLeakCheck $ do
      T3 node _ unsub <- runBuilderInDiv' $ do
         { widget } <- detach $ text "foo"
         text "bar"
         widget
         text "baz"
         widget

      liftEffect (innerHTML node) `shouldReturn`
        """barbazfoo"""

      -- clean up
      liftEffect unsub

    it "works inside dynamic_" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic unit
      T3 node _ unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ text "foo"
         dynamic_ $ map (\_ -> widget) dyn

      liftEffect (innerHTML node) `shouldReturn` """foo"""

      liftEffect $ updateDyn unit

      liftEffect (innerHTML node) `shouldReturn` """foo"""

      -- clean up
      liftEffect unsub

    it "dynamic_ -> attach -> dynamic_" $ withLeakCheck $ do
      Tuple dyn updateDyn <- liftEffect $ newDynamic unit
      Tuple innerDyn updateInnerDyn <- liftEffect $ newDynamic $ text "inner1"
      T3 node _ unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ dynamic_ innerDyn
         dynamic_ $ map (\_ -> widget) dyn

      liftEffect (innerHTML node) `shouldReturn` """inner1"""

      liftEffect $ updateInnerDyn $ text "inner2"
      liftEffect (innerHTML node) `shouldReturn` """inner2"""

      liftEffect $ updateDyn unit
      liftEffect (innerHTML node) `shouldReturn` """inner2"""

      liftEffect $ updateInnerDyn $ text "inner3"
      liftEffect (innerHTML node) `shouldReturn` """inner3"""

      -- clean up
      liftEffect unsub

  it "supports element namespaces" $ withLeakCheck $ do
    let xmlns = "http://www.w3.org/2000/svg"
    Tuple _ (Tuple svgNode _) <- runBuilderInDiv $
      elDynAttrNS' (Just xmlns) "svg" (pure mempty) (pure unit)

    (unsafeCoerce svgNode).namespaceURI `shouldEqual` xmlns

newDynamic :: forall a. a -> Effect (Tuple (Dynamic a) (a -> Effect Unit))
newDynamic initial = do
  {dynamic, set} <- FRP.newDynamic initial
  pure (Tuple dynamic set)
