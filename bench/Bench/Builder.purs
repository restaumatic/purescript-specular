module Bench.Builder
  ( builderTests
  ) where

import Prelude

import Bench.Types (Tests)
import Control.Monad.Reader (runReaderT)
import Data.List.Lazy (replicateM)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (elAttr, elDynAttr, text)
import Specular.Dom.Element as E
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

-- | The same widget, but monomorphic. Can be used to test the effect of
-- | polymorphism to some degree.
staticWidgetMonoOptReplicate :: Int -> Widget Unit
staticWidgetMonoOptReplicate n =
  replicateM_Widget_ n $
    elAttr "div" ("class" := "foo") $ do
      elAttr "div" ("class" := "bar") $ do
        text "foo"
      elAttr "div" ("class" := "baz") $ do
        text "foo"
      elAttr "div" ("class" := "thud") $ do
        text "foo"

staticWidgetMonoOptReplicateD :: Int -> Widget Unit
staticWidgetMonoOptReplicateD n =
  replicateM_Widget_ n $
    elDynAttr "div" (pure $ "class" := "foo") do
      elDynAttr "div" (pure $ "class" := "bar") do
        text "foo"
      elDynAttr "div" (pure $ "class" := "baz") do
        text "foo"
      elDynAttr "div" (pure $ "class" := "thud") do
        text "foo"

foreign import replicateM_Widget_ :: Int -> Widget Unit -> Widget Unit

staticWidgetNewApi :: Int -> Widget Unit
staticWidgetNewApi n =
  replicateM_Widget_ n $
    E.el "div" [E.attrs ("class" := "foo")] do
      E.el "div" [E.attrs ("class" := "bar")] do
        E.text "foo"
      E.el "div" [E.attrs ("class" := "baz")] do
        E.text "foo"
      E.el "div" [E.attrs ("class" := "thud")] do
        E.text "foo"

staticWidgetNewApiD :: Int -> Widget Unit
staticWidgetNewApiD n =
  replicateM_Widget_ n $
    E.el "div" [E.attrsD (pure ("class" := "foo"))] do
      E.el "div" [E.attrsD (pure ("class" := "bar"))] do
        E.text "foo"
      E.el "div" [E.attrsD (pure ("class" := "baz"))] do
        E.text "foo"
      E.el "div" [E.attrsD (pure ("class" := "thud"))] do
        E.text "foo"

-- See comments in the FFI module.
foreign import staticJS :: Int -> Effect Unit
foreign import staticJS_c :: Int -> Effect Unit
foreign import staticJS_m :: Int -> Effect Unit

builderTests :: Tests
builderTests =
  [ Tuple "js                         " (pure $ delay \_ -> staticJS 10)
  , Tuple "js curried                 " (pure $ delay \_ -> staticJS_c 10)
  , Tuple "js monad                   " (pure $ delay \_ -> staticJS_m 10)
  , Tuple "Widget + elAttr            " (pure $ delay \_ -> runWidget $ staticWidgetMonoOptReplicate 10)
  , Tuple "Widget + elDynAttr         " (pure $ delay \_ -> runWidget $ staticWidgetMonoOptReplicateD 10)
  , Tuple "Widget + attr              " (pure $ delay \_ -> runWidget $ staticWidgetNewApi 10)
  , Tuple "Widget + attrD             " (pure $ delay \_ -> runWidget $ staticWidgetNewApiD 10)
  , Tuple "MonadWidget + ReaderT      " (pure $ delay \_ -> runWidget $ runReaderT (staticWidget 10) unit)
  , Tuple "MonadWidget + 2x ReaderT   " (pure $ delay \_ -> runWidget $ flip runReaderT unit $ flip runReaderT unit $ staticWidget 10)
  ]

  where delay x = pure unit >>= x

-- mechanics

runBuilderInDiv' :: forall a. Widget a -> Effect (T3 Node a (Effect Unit))
runBuilderInDiv' widget = do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

runWidget :: forall a. Widget a -> Effect Unit
runWidget w = void $ runBuilderInDiv' w
