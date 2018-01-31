module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync (IOSync)
import Data.IORef (newIORef)
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (detach, domEventWithSample, el, elDynAttr, elDynAttr', rawHtml, text)
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, WeakDynamic, dynamic_, for, holdDyn, never, newEvent, subscribeEvent_, switch, weaken)
import Specular.FRP.Replaceable (dynamic, weakDynamic)
import Specular.FRP.WeakDynamic (switchWeakDyn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (runBuilderInDiv, dispatchTrivialEvent, querySelector)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Builder" $ do
  it "builds static DOM" $ do
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" (pure $ SM.singleton "class" "content") $ do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         rawHtml """<p class="foo">Raw</p>"""
         text "baz"
       elDynAttr "span" (pure mempty) $ pure unit

    ioSync (innerHTML node) `shouldReturn`
      """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><span></span>"""

  it "updates attributes" $ do
    Tuple dyn updateDyn <- ioSync $ newDynamic $
      SM.fromFoldable [ Tuple "k1" "v1", Tuple "k2" "v2" ]
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" (weaken dyn) $ pure unit

    ioSync (innerHTML node) `shouldReturn`
      """<div k1="v1" k2="v2"></div>"""

    ioSync $ updateDyn $ SM.fromFoldable [ Tuple "k1" "v1.1", Tuple "k3" "v3" ]

    ioSync (innerHTML node) `shouldReturn`
      """<div k1="v1.1" k3="v3"></div>"""

  describe "dynamic_" $ do
    it "simple" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """bar"""

    it "surrounded by other elements" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         elDynAttr "span" (pure mempty) $ pure unit
         dynamic_ dyn
         elDynAttr "span" (pure mempty) $ pure unit

      ioSync (innerHTML node) `shouldReturn`
        """<span></span>foo<span></span>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """<span></span>bar<span></span>"""

    it "two subscriptions to the same Dynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ dyn
         dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foofoo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """barbar"""

    it "nested, same Dynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ $ dyn $> dynamic_ dyn

      ioSync (innerHTML node) `shouldReturn`
        """foo"""

      ioSync $ updateDyn $ text "bar"

      ioSync (innerHTML node) `shouldReturn`
        """bar"""

    it "nested, different Dynamics" $ do
      Tuple dyn1 updateDyn1 <- ioSync $ newDynamic $ text "foo1"
      Tuple dyn2 updateDyn2 <- ioSync $ newDynamic $ text "foo2"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ $ map (\x -> x *> dynamic_ dyn2) dyn1

      ioSync (innerHTML node) `shouldReturn`
        """foo1foo2"""

      ioSync $ updateDyn1 $ text "bar1"

      ioSync (innerHTML node) `shouldReturn`
        """bar1foo2"""

      ioSync $ updateDyn2 $ text "bar2"

      ioSync (innerHTML node) `shouldReturn`
        """bar1bar2"""

    it "with rawHtml" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ rawHtml "<p>raw</p>"
      Tuple node result <- runBuilderInDiv $ do
        el "br" $ pure unit
        dynamic_ dyn
        el "br" $ pure unit

      ioSync (innerHTML node) `shouldReturn`
        """<br><p>raw</p><br>"""

      ioSync $ updateDyn $ rawHtml "<input>"

      ioSync (innerHTML node) `shouldReturn`
        """<br><input><br>"""

  describe "domEventWithSample" $ do
    it "dispatches DOM events and handles unsubscribe" $ do
      Tuple node {button,event} <- runBuilderInDiv $ do
        Tuple button _ <- elDynAttr' "button" (pure mempty) (text "foo")
        event <- domEventWithSample (\_ -> pure unit) "click" button
        pure {button,event}

      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $ subscribeEvent_ (append log) event

      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

  describe "integration test - `dynamic`/`weakDynamic` and flattening" $ do
    it "clearCompletedButton from TodoMVC, using Dynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic false
      let
        anyCompletedTasks :: Dynamic Boolean
        anyCompletedTasks = dyn

      Tuple node (result :: Dynamic (Event Unit)) <- runBuilderInDiv $
        dynamic $ for anyCompletedTasks $ \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switch result

      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $ subscribeEvent_ (append log) event

      ioSync (innerHTML node) `shouldReturn` ""

      ioSync $ updateDyn true
      ioSync (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- ioSync $ querySelector "button" node
      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

    it "clearCompletedButton from TodoMVC, using WeakDynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic false
      let
        anyCompletedTasks :: WeakDynamic Boolean
        anyCompletedTasks = weaken dyn

      Tuple node (result :: WeakDynamic (Event Unit)) <- runBuilderInDiv $
        weakDynamic $ for anyCompletedTasks $ \anyCompletedTasks' ->
          if anyCompletedTasks'
            then buttonOnClick (pure mempty) (text "Clear")
            else pure never

      let
        event :: Event Unit
        event = switchWeakDyn result

      log <- ioSync $ newIORef []
      _ <- ioSync $ runCleanupT $ subscribeEvent_ (append log) event

      ioSync (innerHTML node) `shouldReturn` ""

      ioSync $ updateDyn true
      ioSync (innerHTML node) `shouldReturn`
        """<button>Clear</button>"""

      button <- ioSync $ querySelector "button" node
      ioSync $ dispatchTrivialEvent button "click"
      log `shouldHaveValue` [unit]

  describe "detach" $ do
    it "simple case" $ do
      Tuple node result <- runBuilderInDiv $ do
         { value, widget } <- detach $ text "foo" *> pure 7
         text "bar"
         widget
         pure value

      result `shouldEqual` 7

      ioSync (innerHTML node) `shouldReturn`
        """barfoo"""

    it "double use" $ do
      Tuple node _ <- runBuilderInDiv $ do
         { widget } <- detach $ text "foo"
         text "bar"
         widget
         text "baz"
         widget

      ioSync (innerHTML node) `shouldReturn`
        """barbazfoo"""

    it "works inside dynamic_" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic unit
      Tuple node _ <- runBuilderInDiv $ do
         { value, widget } <- detach $ text "foo"
         dynamic_ $ map (\_ -> widget) dyn

      ioSync (innerHTML node) `shouldReturn` """foo"""

      ioSync $ updateDyn unit

      ioSync (innerHTML node) `shouldReturn` """foo"""

    it "dynamic_ -> attach -> dynamic_" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic unit
      Tuple innerDyn updateInnerDyn <- ioSync $ newDynamic $ text "inner1"
      Tuple node _ <- runBuilderInDiv $ do
         { value, widget } <- detach $ dynamic_ innerDyn
         dynamic_ $ map (\_ -> widget) dyn

      ioSync (innerHTML node) `shouldReturn` """inner1"""

      ioSync $ updateInnerDyn $ text "inner2"
      ioSync (innerHTML node) `shouldReturn` """inner2"""

      ioSync $ updateDyn unit
      ioSync (innerHTML node) `shouldReturn` """inner2"""

      ioSync $ updateInnerDyn $ text "inner3"
      ioSync (innerHTML node) `shouldReturn` """inner3"""

newDynamic :: forall a. a -> IOSync (Tuple (Dynamic a) (a -> IOSync Unit))
newDynamic initial = do
  {event,fire} <- newEvent
  Tuple dyn _ <- runCleanupT $ holdDyn initial event
  pure (Tuple dyn fire)
