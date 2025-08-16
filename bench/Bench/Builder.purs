module Bench.Builder
  ( builderTests
  ) where

import Prelude

import Bench.Types (Tests)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Specular.Dom.Browser (Node, createElement, (:=))
import Specular.Dom.Element as E
import Specular.Dom.Widget (Widget, runWidgetInNode)
import Test.Utils.Dom (T3(T3))

foreign import replicateM_Widget_ :: Int -> Widget Unit -> Widget Unit

staticWidgetNewApi :: Int -> Widget Unit
staticWidgetNewApi n =
  replicateM_Widget_ n $
    E.el "div" [ E.attrs ("class" := "foo") ] do
      E.el "div" [ E.attrs ("class" := "bar") ] do
        E.text "foo"
      E.el "div" [ E.attrs ("class" := "baz") ] do
        E.text "foo"
      E.el "div" [ E.attrs ("class" := "thud") ] do
        E.text "foo"

staticWidgetNewApiD :: Int -> Widget Unit
staticWidgetNewApiD n =
  replicateM_Widget_ n $
    E.el "div" [ E.attrsD (pure ("class" := "foo")) ] do
      E.el "div" [ E.attrsD (pure ("class" := "bar")) ] do
        E.text "foo"
      E.el "div" [ E.attrsD (pure ("class" := "baz")) ] do
        E.text "foo"
      E.el "div" [ E.attrsD (pure ("class" := "thud")) ] do
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
  , Tuple "Widget + attr              " (pure $ delay \_ -> runWidget $ staticWidgetNewApi 10)
  , Tuple "Widget + attrD             " (pure $ delay \_ -> runWidget $ staticWidgetNewApiD 10)
  ]

  where
  delay x = pure unit >>= x

-- mechanics

runBuilderInDiv' :: forall a. Widget a -> Effect (T3 Node a (Effect Unit))
runBuilderInDiv' widget = do
  parent <- createElement "div"
  Tuple result unsub <- runWidgetInNode parent widget
  pure (T3 parent result unsub)

runWidget :: forall a. Widget a -> Effect Unit
runWidget w = void $ runBuilderInDiv' w
