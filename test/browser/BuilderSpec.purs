module BuilderSpec where

import Prelude hiding (append)

import Control.Monad.Cleanup (execCleanupT)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Specular.Dom.Browser (innerHTML)
import Specular.Dom.Builder.Class (domEventWithSample, el, elAttr, elDynAttr, elDynAttr', elDynAttrNS', rawHtml, text)
import Specular.Dom.Element (dynText)
import Specular.Dom.Node.Class ((:=))
import Specular.Dom.Widgets.Button (buttonOnClick)
import Specular.FRP (Dynamic, Event, WeakDynamic, dynamic_, never, subscribeEvent_, switch, weaken, whenJustD, whenD, unlessD)
import Specular.FRP as FRP
import Specular.FRP.Replaceable (dynamic, weakDynamic)
import Specular.FRP.WeakDynamic (switchWeakDyn)
import Specular.Internal.Effect (modifyRef, newRef)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (append, liftEffect, shouldHaveValue, shouldReturn, withLeakCheck)
import Test.Utils.Dom (T3(..), dispatchTrivialEvent, querySelector, runBuilderInDiv, runBuilderInDiv')
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec = describe "Builder" do
  it "builds static DOM" $ withLeakCheck do
    T3 node result unsub <- runBuilderInDiv' do
       elAttr "div" ("class" := "content") do
         text "foo"
         elDynAttr "span" (pure mempty) $ text "bar"
         rawHtml """<p class="foo">Raw</p>"""
         text "baz"
       elAttr "span" mempty $ pure unit

    liftEffect (innerHTML node) `shouldReturn`
      """<div class="content">foo<span>bar</span><p class="foo">Raw</p>baz</div><span></span>"""

    -- clean up
    liftEffect unsub

  it "updates attributes" $ withLeakCheck do
    Tuple dyn updateDyn <- liftEffect $ newDynamic $ "k1" := "v1" <> "k2" := "v2"
    T3 node result unsub <- runBuilderInDiv' do
       elDynAttr "div" (weaken dyn) $ pure unit

    liftEffect (innerHTML node) `shouldReturn`
      """<div k2="v2" k1="v1"></div>"""

    liftEffect $ updateDyn $ "k1" := "v1.1" <> "k3" := "v3"

    liftEffect (innerHTML node) `shouldReturn`
      """<div k1="v1.1" k3="v3"></div>"""

    -- clean up
    liftEffect unsub

  describe "dynamic_" do
    it "simple" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' $ dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """bar"""

      -- clean up
      liftEffect unsub

    it "surrounded by other elements" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' do
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

    it "two subscriptions to the same Dynamic" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' do
         dynamic_ dyn
         dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foofoo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """barbar"""

      -- clean up
      liftEffect unsub

    it "nested, same Dynamic" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ text "foo"
      T3 node result unsub <- runBuilderInDiv' do
         dynamic_ $ dyn <#> \d -> do
           d
           dynamic_ dyn

      liftEffect (innerHTML node) `shouldReturn`
        """foofoo"""

      liftEffect $ updateDyn $ text "bar"

      liftEffect (innerHTML node) `shouldReturn`
        """barbar"""

      -- clean up
      liftEffect unsub

    it "nested, different Dynamics" $ withLeakCheck do
      Tuple dyn1 updateDyn1 <- liftEffect $ newDynamic $ text "foo1"
      Tuple dyn2 updateDyn2 <- liftEffect $ newDynamic $ text "foo2"
      T3 node result unsub <- runBuilderInDiv' do
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

    it "with rawHtml" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic $ rawHtml "<p>raw</p>"
      T3 node result unsub <- runBuilderInDiv' do
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


  describe "whenJustD" do
    it "renders empty when dynamic is initially Nothing" $ withLeakCheck do
      Tuple dynMb _  <- liftEffect $ newDynamic Nothing
      T3 node _ unsub <- runBuilderInDiv' do
        elDynAttr "span" (pure mempty) $ pure unit
        whenJustD dynMb $ \dyn -> dynText dyn
        elDynAttr "span" (pure mempty) $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span><span></span>"""

      -- clean up
      liftEffect unsub

    it "renders contents when dynamic is initially Just" $ withLeakCheck do
      Tuple dynMb _  <- liftEffect $ newDynamic $ Just "foobar"
      T3 node _ unsub <- runBuilderInDiv' do
        elDynAttr "span" (pure mempty) $ pure unit
        whenJustD dynMb $ \dyn -> dynText dyn
        elDynAttr "span" (pure mempty) $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>foobar<span></span>"""

      -- clean up
      liftEffect unsub

    it "renders contents when dynamic is initially Nothing then updated to Just" $ withLeakCheck do
      Tuple dynMb updateDyn  <- liftEffect $ newDynamic $ Nothing
      T3 node _ unsub <- runBuilderInDiv' do
        elDynAttr "span" (pure mempty) $ pure unit
        whenJustD dynMb $ \dyn -> dynText dyn
        elDynAttr "span" (pure mempty) $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span><span></span>"""

      liftEffect $ updateDyn $ Just "foobar"
      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>foobar<span></span>"""

      -- clean up
      liftEffect unsub

    it "renders empty when dynamic is initially Just then updated to Nothing" $ withLeakCheck do
      Tuple dynMb updateDyn  <- liftEffect $ newDynamic $ Just "foobar"
      T3 node _ unsub <- runBuilderInDiv' do
        elDynAttr "span" (pure mempty) $ pure unit
        whenJustD dynMb $ \dyn -> dynText dyn
        elDynAttr "span" (pure mempty) $ pure unit

      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>foobar<span></span>"""

      liftEffect $ updateDyn Nothing
      liftEffect (innerHTML node) `shouldReturn`
        """<span></span><span></span>"""

      -- clean up
      liftEffect unsub

    it "does not rerender contents when not necessary" $ withLeakCheck do
      Tuple dynMb updateDyn  <- liftEffect $ newDynamic $ Nothing
      count <- liftEffect $ newRef 0

      T3 node _ unsub <- runBuilderInDiv' do
        elDynAttr "span" (pure mempty) $ pure unit
        whenJustD dynMb $ \dyn -> do
          liftEffect $ modifyRef count (_ + 1)
          dynText dyn
        elDynAttr "span" (pure mempty) $ pure unit

      liftEffect $ updateDyn $ Just "foo"
      liftEffect $ updateDyn $ Just "bar"
      liftEffect $ updateDyn $ Just "foo"
      liftEffect $ updateDyn Nothing
      liftEffect $ updateDyn $ Just "baz"
      liftEffect $ updateDyn $ Just "bar"
      liftEffect $ updateDyn $ Just "baz"
      liftEffect (innerHTML node) `shouldReturn`
        """<span></span>baz<span></span>"""

      count `shouldHaveValue` 2
      -- clean up
      liftEffect unsub

    describe "whenD" do
      it "renders empty when dynamic is initially false" $ withLeakCheck do
        Tuple dynMb _  <- liftEffect $ newDynamic false
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          whenD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect (innerHTML node) `shouldReturn`
          """<span></span><span></span>"""

        -- clean up
        liftEffect unsub

      it "renders contents when dynamic is initially true" $ withLeakCheck do
        Tuple dynMb _  <- liftEffect $ newDynamic true
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          whenD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        -- clean up
        liftEffect unsub

      it "renders contents when dynamic is initially false then updated to true" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic false
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          whenD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn true
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        -- clean up
        liftEffect unsub

      it "renders empty when dynamic is initially true then updated to false" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic true
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          whenD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn false
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span><span></span>"""

        -- clean up
        liftEffect unsub

      it "does not rerender contents when not necessary" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic false
        count <- liftEffect $ newRef 0

        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          whenD dynMb do
            liftEffect $ modifyRef count (_ + 1)
            text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn true
        liftEffect $ updateDyn true
        liftEffect $ updateDyn true
        liftEffect $ updateDyn false
        liftEffect $ updateDyn true
        liftEffect $ updateDyn true
        liftEffect $ updateDyn true
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        count `shouldHaveValue` 2
        -- clean up
        liftEffect unsub

    describe "unlessD" do
      it "renders empty when dynamic is initially true" $ withLeakCheck do
        Tuple dynMb _  <- liftEffect $ newDynamic true
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          unlessD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect (innerHTML node) `shouldReturn`
          """<span></span><span></span>"""

        -- clean up
        liftEffect unsub

      it "renders contents when dynamic is initially false" $ withLeakCheck do
        Tuple dynMb _  <- liftEffect $ newDynamic false
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          unlessD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        -- clean up
        liftEffect unsub

      it "renders contents when dynamic is initially true then updated to false" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic true
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          unlessD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn false
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        -- clean up
        liftEffect unsub

      it "renders empty when dynamic is initially false then updated to true" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic false
        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          unlessD dynMb $ text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn true
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span><span></span>"""

        -- clean up
        liftEffect unsub

      it "does not rerender contents when not necessary" $ withLeakCheck do
        Tuple dynMb updateDyn  <- liftEffect $ newDynamic true
        count <- liftEffect $ newRef 0

        T3 node _ unsub <- runBuilderInDiv' do
          elDynAttr "span" (pure mempty) $ pure unit
          unlessD dynMb do
            liftEffect $ modifyRef count (_ + 1)
            text "hello"
          elDynAttr "span" (pure mempty) $ pure unit

        liftEffect $ updateDyn false
        liftEffect $ updateDyn false
        liftEffect $ updateDyn false
        liftEffect $ updateDyn true
        liftEffect $ updateDyn false
        liftEffect $ updateDyn false
        liftEffect $ updateDyn false
        liftEffect (innerHTML node) `shouldReturn`
          """<span></span>hello<span></span>"""

        count `shouldHaveValue` 2
        -- clean up
        liftEffect unsub

  describe "domEventWithSample" do
    it "dispatches DOM events and handles unsubscribe" $ withLeakCheck do
      T3 node {button,event} unsub1 <- runBuilderInDiv' do
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

  describe "integration test - `dynamic`/`weakDynamic` and flattening" do
    it "clearCompletedButton from TodoMVC, using Dynamic" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic false
      let
        anyCompletedTasks :: Dynamic Boolean
        anyCompletedTasks = dyn

      T3 node (result :: Dynamic (Event Unit)) unsub1 <- runBuilderInDiv' do
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

    it "clearCompletedButton from TodoMVC, using WeakDynamic" $ withLeakCheck do
      Tuple dyn updateDyn <- liftEffect $ newDynamic false
      let
        anyCompletedTasks :: WeakDynamic Boolean
        anyCompletedTasks = weaken dyn

      T3 node (result :: WeakDynamic (Event Unit)) unsub1 <- runBuilderInDiv' do
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

  it "supports element namespaces" $ withLeakCheck do
    let xmlns = "http://www.w3.org/2000/svg"
    T3 _ (Tuple svgNode _) unsub <- runBuilderInDiv' $
      elDynAttrNS' (Just xmlns) "svg" (pure mempty) (pure unit)

    (unsafeCoerce svgNode).namespaceURI `shouldEqual` xmlns

    -- clean up
    liftEffect unsub

newDynamic :: forall a. a -> Effect (Tuple (Dynamic a) (a -> Effect Unit))
newDynamic initial = do
  {dynamic, set} <- FRP.newDynamic initial
  pure (Tuple dynamic set)
