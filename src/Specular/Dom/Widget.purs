module Specular.Dom.Widget (
    Widget
  , RWidget
  , runWidgetInNode 
  , runMainWidgetInNode
  , runMainWidgetInBody

  , class MonadWidget

  , liftWidget
) where

import Prelude

import Control.Monad.Replace (class MonadReplace)
import Data.Tuple (Tuple, fst)
import Effect (Effect)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, runBuilder, unBuilder)
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder, liftBuilder)
import Specular.FRP (class MonadFRP)
import Specular.Internal.RIO (RIO(..))

type Widget = RWidget Unit

type RWidget = Builder

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
class (MonadDomBuilder Unit m, MonadFRP m, MonadReplace m, MonadDetach m, Monoid (m Unit)) <= MonadWidget m
instance monadWidget :: (MonadDomBuilder Unit m, MonadFRP m, MonadReplace m, MonadDetach m, Monoid (m Unit)) => MonadWidget m

-- | Lift a `Widget` into any `MonadWidget` monad.
liftWidget :: forall m env a. MonadDomBuilder env m => RWidget env a -> m a
liftWidget w = let RIO f = unBuilder w in liftBuilder f
