module Specular.Dom.Widget (
    Widget
  , runWidgetInNode 
  , runMainWidgetInNode
  , runMainWidgetInBody

  , class MonadWidget
) where

import Prelude

import Effect (Effect)
import Control.Monad.Replace (class MonadReplace)
import Data.Tuple (Tuple, fst)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, runBuilder)
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder)
import Specular.FRP (class MonadFRP)

type Widget = Builder Node

-- | Runs a widget in the specified parent element. Returns the result and cleanup action.
runWidgetInNode :: forall a. Node -> Widget a -> Effect (Tuple a (Effect Unit))
runWidgetInNode parent widget = runBuilder parent widget

-- | Runs a widget in the specified parent element and discards cleanup action.
runMainWidgetInNode :: forall a. Node -> Widget a -> Effect a
runMainWidgetInNode parent widget = fst <$> runWidgetInNode parent widget

-- | Runs a widget in `document.body` and discards cleanup action.
runMainWidgetInBody :: forall a. Widget a -> Effect a
runMainWidgetInBody widget = do
  body <- documentBody
  runMainWidgetInNode body widget

foreign import documentBody :: Effect Node

-- A handy alias for all the constraints you'll need
class (MonadDomBuilder m, MonadFRP m, MonadReplace m, MonadDetach m, Monoid (m Unit)) <= MonadWidget m
instance monadWidget :: (MonadDomBuilder m, MonadFRP m, MonadReplace m, MonadDetach m, Monoid (m Unit)) => MonadWidget m
