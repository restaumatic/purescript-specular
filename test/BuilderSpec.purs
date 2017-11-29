module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Aff (Aff)
import Control.Monad.Cleanup (runCleanupT)
import Control.Monad.IOSync (IOSync)
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node, outerHTML)
import Specular.Dom.Node.Class (createElement)
import Specular.Dom.Widget (Builder, dynamic_, elDynAttr, runBuilder, text)
import Specular.FRP (Dynamic, holdDyn, newEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Utils (ioSync, shouldReturn)

spec :: forall eff. Spec (RunnerEffects eff) Unit
spec = describe "Builder" $ do
  it "builds static DOM" $ do
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" (pure $ SM.singleton "class" "content") $ do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         text "baz"
       elDynAttr "span" (pure mempty) $ pure unit

    ioSync (outerHTML node) `shouldReturn`
      """<div><div class="content">foo<span>bar</span>baz</div><span></span></div>"""

  it "updates attributes" $ do
    Tuple dyn updateDyn <- ioSync $ newDynamic $
      SM.fromFoldable [ Tuple "k1" "v1", Tuple "k2" "v2" ]
    Tuple node result <- runBuilderInDiv $ do
       elDynAttr "div" dyn $ pure unit

    ioSync (outerHTML node) `shouldReturn`
      """<div><div k1="v1" k2="v2"></div></div>"""

    ioSync $ updateDyn $ SM.fromFoldable [ Tuple "k1" "v1.1", Tuple "k3" "v3" ]

    ioSync (outerHTML node) `shouldReturn`
      """<div><div k1="v1.1" k3="v3"></div></div>"""

  describe "dynamic_" $ do
    it "simple" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ dynamic_ dyn

      ioSync (outerHTML node) `shouldReturn`
        """<div>foo</div>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (outerHTML node) `shouldReturn`
        """<div>bar</div>"""

    it "surrounded by other elements" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         elDynAttr "span" (pure mempty) $ pure unit
         dynamic_ dyn
         elDynAttr "span" (pure mempty) $ pure unit

      ioSync (outerHTML node) `shouldReturn`
        """<div><span></span>foo<span></span></div>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (outerHTML node) `shouldReturn`
        """<div><span></span>bar<span></span></div>"""

    it "two subscriptions to the samy Dynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ dyn
         dynamic_ dyn

      ioSync (outerHTML node) `shouldReturn`
        """<div>foofoo</div>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (outerHTML node) `shouldReturn`
        """<div>barbar</div>"""

    it "nested, same Dynamic" $ do
      Tuple dyn updateDyn <- ioSync $ newDynamic $ text "foo"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ $ dyn $> dynamic_ dyn

      ioSync (outerHTML node) `shouldReturn`
        """<div>foo</div>"""

      ioSync $ updateDyn $ text "bar"

      ioSync (outerHTML node) `shouldReturn`
        """<div>bar</div>"""

    it "nested, different Dynamics" $ do
      Tuple dyn1 updateDyn1 <- ioSync $ newDynamic $ text "foo1"
      Tuple dyn2 updateDyn2 <- ioSync $ newDynamic $ text "foo2"
      Tuple node result <- runBuilderInDiv $ do
         dynamic_ $ map (\x -> x *> dynamic_ dyn2) dyn1

      ioSync (outerHTML node) `shouldReturn`
        """<div>foo1foo2</div>"""

      ioSync $ updateDyn1 $ text "bar1"

      ioSync (outerHTML node) `shouldReturn`
        """<div>bar1foo2</div>"""

      ioSync $ updateDyn2 $ text "bar2"

      ioSync (outerHTML node) `shouldReturn`
        """<div>bar1bar2</div>"""

newDynamic :: forall a. a -> IOSync (Tuple (Dynamic a) (a -> IOSync Unit))
newDynamic initial = do
  {event,fire} <- newEvent
  Tuple dyn _ <- runCleanupT $ holdDyn initial event
  pure (Tuple dyn fire)

runBuilderInDiv :: forall r a. Builder Node a -> Aff r (Tuple Node a)
runBuilderInDiv builder = ioSync $ do
  parent <- createElement "div"
  Tuple result _ <- runBuilder {parent} builder
  pure (Tuple parent result)
