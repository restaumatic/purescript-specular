module Bench.Builder
  ( builderTests
  ) where

import Prelude

import Bench.Types (Tests)
import Control.Monad.Eff (Eff)
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (IOSync, runIOSync')
import Control.Monad.Reader (runReaderT)
import Data.List.Lazy (replicateM)
import Data.Tuple (Tuple(Tuple))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (elAttr, text)
import Specular.Dom.Node.Class (createElement, (:=))
import Specular.Dom.Widget (class MonadWidget, Widget, runWidgetInNode)
import Test.Utils.Dom (T3(T3))

-- | The widget we're rendering.
staticWidget :: forall m. MonadWidget m => Int -> m Unit
staticWidget n =
  void $ replicateM n $
    elAttr "div" ("class" := "foo") $ do
      elAttr "div" ("class" := "bar") $ do
        text "foo"
      elAttr "div" ("class" := "baz") $ do
        text "foo"
      elAttr "div" ("class" := "thud") $ do
        text "foo"

-- | The same widget, but monomorphic. Can be used to test the effect of
-- | polymorphism to some degree.
staticWidgetMono :: Int -> Widget Unit
staticWidgetMono n =
  void $ replicateM n $
    elAttr "div" ("class" := "foo") $ do
      elAttr "div" ("class" := "bar") $ do
        text "foo"
      elAttr "div" ("class" := "baz") $ do
        text "foo"
      elAttr "div" ("class" := "thud") $ do
        text "foo"

-- See comments in the FFI module.
foreign import staticJS :: forall e. Int -> Eff e Unit
foreign import staticJS_c :: forall e. Int -> Eff e Unit
foreign import staticJS_m :: forall e. Int -> Eff e Unit

builderTests :: Tests
builderTests =
  [ Tuple "js 10" (pure $ staticJS 10)
  , Tuple "js_c 10" (pure $ staticJS_c 10)
  , Tuple "js_m 10" (pure $ staticJS_m 10)
  , Tuple "static mono 10" (pure $ runWidget $ staticWidgetMono 10)
  , Tuple "static 10" (pure $ runWidget $ deoptimizeWidget (staticWidget 10))
  , Tuple "static ReaderT 10"
      (pure $ runWidget $ deoptimizeWidget (runReaderT (staticWidget 10) unit))
  , Tuple "static 2x ReaderT 10"
      (pure $ runWidget $ deoptimizeWidget
        (flip runReaderT unit $ flip runReaderT unit $ staticWidget 10))
  ]


-- mechanics

runBuilderInDiv' :: forall a. Widget a -> IOSync (T3 Node a (IOSync Unit))
runBuilderInDiv' widget = do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

runWidget :: forall a e. Widget a -> Eff (infinity :: INFINITY | e) Unit
runWidget w = void $ runIOSync' $ runBuilderInDiv' w

-- | Identity function, but prevents purs-opt from monomorphizing the widget.
-- | Used to test the impact of monomorphization on Builder.
deoptimizeWidget :: forall a. (forall m. MonadWidget m => m a) -> Widget a
deoptimizeWidget x = x
