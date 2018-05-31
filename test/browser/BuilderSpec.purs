module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT, runCleanupT)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.IOSync (IOSync)
import Specular.Internal.Effect (newRef)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (detach, domEventWithSample, el, elDynAttr, elDynAttr', elDynAttrNS', rawHtml, text)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, WeakDynamic, dynamic_, for, holdDyn, never, newEvent, subscribeEvent_, switch, weaken)
import Specular.FRP.Replaceable (dynamic, weakDynamic)
import Specular.FRP.WeakDynamic (switchWeakDyn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, modifyTotalListeners, shouldHaveValue, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), dispatchTrivialEvent, querySelector, runBuilderInDiv, runBuilderInDiv')
import Unsafe.Coerce (unsafeCoerce)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Builder" $ do
  it "builds static DOM" $ withLeakCheck $ do
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" (pure $ SM.singleton "class" "content") $ do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         rawHtml """<p class="foo">Raw</p>"""
         text "baz"
       elDynAttr "span" (pure mempty) $ pure unit

    ioSync (innerHTML node) `shouldReturn`
      """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><span></span>"""

  it "updates attributes" $ withLeakCheck $ do
    Tuple dyn updateDyn <- ioSync $ newDynamic $
      SM.fromFoldable [ Tuple "k1" "v1", Tuple "k2" "v2" ]
    T3 node result unsub <- runBuilderInDiv' $ do
       elDynAttr "div" (weaken dyn) $ pure unit

    ioSync (innerHTML node) `shouldReturn`
      """<div k1="v1" k2="v2"></div>"""

    ioSync $ updateDyn $ SM.fromFoldable [ Tuple "k1" "v1.1", Tuple "k3" "v3" ]

    ioSync (innerHTML node) `shouldReturn`
      """<div k1="v1.1" k3="v3"></div>"""

    -- clean up
    ioSync unsub

  describe "dynamic_" $ do
    it "simple" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """bar"""

      -- clean up
      ioSync unsub

    it "surrounded by other elements" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         elDynAttr "span" (pure mempty) $ pure unit
         dynamic_ dyn
         elDynAttr "span" (pure mempty) $ pure unit

      ioSync (innerHTML node) `shouldReturn`
        """<span></span>foo<span></span>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """<span></span>bar<span></span>"""

      -- clean up
      ioSync unsub

    it "two subscriptions to the same Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ dyn
         dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foofoo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """barbar"""

      -- clean up
      ioSync unsub

    it "nested, same Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ $ dyn $> dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """bar"""

      -- clean up
      ioSync unsub

    it "nested, different Dynamics" $ withLeakCheck $ do
      Tuple dyn1 updateDyn1 <- ioSync $ newDynamic $ text "foo1"
      Tuple dyn2 updateDyn2 <- ioSync $ newDynamic $ text "foo2"
      T3 node result unsub <- runBuilderInDiv' $ do
         dynamic_ $ map (\x -> x *> dynamic_ dyn2) dyn1

      ioSync (innerHTML node) `shouldReturn`
        """foo1foo2"""

      ioSync $ updateDyn1 $ text "bar1"

      ioSync (innerHTML node) `shouldReturn`
        """bar1foo2"""

      ioSync $ updateDyn2 $ text "bar2"

      ioSync (innerHTML node) `shouldReturn`
        """bar1bar2"""

      -- clean up
      ioSync unsub

    it "with rawHtml" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ rawHtml "<p>raw</p>"
      T3 node result unsub <- runBuilderInDiv' $ do
        el "br" $ pure unit
        dynamic_ dyn
        el "br" $ pure unit

      ioSync (innerHTML node) `shouldReturn`
        """<br><p>raw</p><br>"""

      ioSync $ updateDyn $ rawHtml "<input>"

      ioSync (innerHTML node) `shouldReturn`
        """<br><input><br>"""

      -- clean up
      ioSync unsub

  describe "domEventWithSample" $ do
    it "dispatches DOM events and handles unsubscribe" $ withLeakCheck $ do
      T3 node {button,event} unsub1 <- runBuilderInDiv' $ do
        Tuple button _ <- elDynAttr' "button" (pure mempty) (text "foo")
        event <- domEventWithSample (\_ -> pure unit) "click" button
        pure {button,event}

      log <- ioSync $ newRef []
      unsub2 <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      ioSync unsub1
      ioSync unsub2

  describe "integration test - `dynamic`/`weakDynamic` and flattening" $ do
    it "clearCompletedButton from TodoMVC, using Dynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic false
      let
        anyCompletedTasks :: Dynamic Boolean
        anyCompletedTasks = dyn

      T3 node (result :: Dynamic (Event Unit)) unsub1 <- runBuilderInDiv' $ do
        dynamic $ for anyCompletedTasks $ \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switch result

      log <- ioSync $ newRef []
      unsub2 <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      ioSync (innerHTML node) `shouldReturn` ""

      ioSync $ updateDyn true
      ioSync (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- ioSync $ querySelector "button" node
      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      ioSync unsub1
      ioSync unsub2

    it "clearCompletedButton from TodoMVC, using WeakDynamic" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic false
      let
        anyCompletedTasks :: WeakDynamic Boolean
        anyCompletedTasks = weaken dyn

      T3 node (result :: WeakDynamic (Event Unit)) unsub1 <- runBuilderInDiv' $ do
        weakDynamic $ for anyCompletedTasks $ \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switchWeakDyn result

      log <- ioSync $ newRef []
      unsub2 <- ioSync $ execCleanupT $ subscribeEvent_ (append log) event

      ioSync (innerHTML node) `shouldReturn` ""

      ioSync $ updateDyn true
      ioSync (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- ioSync $ querySelector "button" node
      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

      -- clean up
      ioSync unsub1
      ioSync unsub2

  describe "detach" $ do
    it "simple case" $ withLeakCheck $ do
      T3 node result unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ text "foo" *> pure 7
         text "bar"
         widget
         pure value

      result `shouldEqual` 7

      ioSync (innerHTML node) `shouldReturn`
        """barfoo"""

      -- clean up
      ioSync unsub

    it "double use" $ withLeakCheck $ do
      T3 node _ unsub <- runBuilderInDiv' $ do
         { widget } <- detach $ text "foo"
         text "bar"
         widget
         text "baz"
         widget

      ioSync (innerHTML node) `shouldReturn`
        """barbazfoo"""

      -- clean up
      ioSync unsub

    it "works inside dynamic_" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic unit
      T3 node _ unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ text "foo"
         dynamic_ $ map (\_ -> widget) dyn

      ioSync (innerHTML node) `shouldReturn` """foo"""

      ioSync $ updateDyn unit

      ioSync (innerHTML node) `shouldReturn` """foo"""

      -- clean up
      ioSync unsub

    it "dynamic_ -> attach -> dynamic_" $ withLeakCheck $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic unit
      Tuple innerDyn updateInnerDyn <- ioSync $ newDynamic $ text "inner1"
      T3 node _ unsub <- runBuilderInDiv' $ do
         { value, widget } <- detach $ dynamic_ innerDyn
         dynamic_ $ map (\_ -> widget) dyn

      ioSync (innerHTML node) `shouldReturn` """inner1"""

      ioSync $ updateInnerDyn $ text "inner2"
      ioSync (innerHTML node) `shouldReturn` """inner2"""

      ioSync $ updateDyn unit
      ioSync (innerHTML node) `shouldReturn` """inner2"""

      ioSync $ updateInnerDyn $ text "inner3"
      ioSync (innerHTML node) `shouldReturn` """inner3"""

      -- clean up
      ioSync unsub

  it "supports element namespaces" $ withLeakCheck $ do
    let xmlns = "http://www.w3.org/2000/svg"
    Tuple _ (Tuple svgNode _) <- runBuilderInDiv $
      elDynAttrNS' (Just xmlns) "svg" (pure mempty) (pure unit)

    (unsafeCoerce svgNode).namespaceURI `shouldEqual` xmlns

newDynamic :: forall a. a -> IOSync (Tuple (Dynamic a) (a -> IOSync Unit))
newDynamic initial = do
  {event,fire} <- newEvent
  Tuple dyn _ <- runCleanupT $ holdDyn initial event

  -- HACK: We're discarding the cleanup here,
  -- but to avoid threading it through we just decrement the counter manually.
  liftEff $ modifyTotalListeners (_ - 1)
  pure (Tuple dyn fire)
