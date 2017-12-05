module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync (IOSync)
import Data.IORef (newIORef)
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr, elDynAttr', text)
import Specular.FRP (Dynamic, dynamic_, holdDyn, newEvent, subscribeEvent_, weaken)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (append, ioSync, shouldHaveValue, shouldReturn)
import Test.Utils.Dom (runBuilderInDiv, dispatchTrivialEvent)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Builder" $ do
  it "builds static DOM" $ do
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" (pure $ SM.singleton "class" "content") $ do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         text "baz"
       elDynAttr "span" (pure mempty) $ pure unit

    ioSync (innerHTML node) `shouldReturn`
      """<div class="content">foo<span>bar</span>baz</div><span></span>"""

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

newDynamic :: forall a. a -> IOSync (Tuple (Dynamic a) (a -> IOSync Unit))
newDynamic initial = do
  {event,fire} <- newEvent
  Tuple dyn _ <- runCleanupT $ holdDyn initial event
  pure (Tuple dyn fire)
