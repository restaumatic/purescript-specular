module Specular.Dom.Widget
  ( Widget
  , RWidget
  , runWidgetInNode
  , runWidgetInBody
  , runMainWidgetInNode
  , runMainWidgetInBody
  , spawnWidgetInNode
  , spawnWidgetInBody

  , class MonadWidget

  , liftWidget
  , emptyWidget
  ) where

import Prelude

import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Uncurried (mkEffectFn1, runEffectFn1)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, runBuilder, unBuilder)
import Specular.Dom.Builder.Class (class MonadDomBuilder, liftBuilder)
import Specular.FRP (class MonadFRP)
import Specular.Internal.RIO (RIO(..))
import Control.Monad.Replace (class MonadReplace, newSlot, replaceSlot, destroySlot)
import Control.Monad.Cleanup (class MonadCleanup, onCleanup)

type Widget = RWidget Unit

type RWidget = Builder

-- | Runs a widget in the specified parent element. Returns the result and cleanup action.
runWidgetInNode :: forall a. Node -> Widget a -> Effect (Tuple a (Effect Unit))
runWidgetInNode parent widget = runBuilder parent do
  slot <- newSlot
  onCleanup (destroySlot slot)
  liftEffect $ replaceSlot slot widget

-- | Runs a widget `document.body`. Returns the result and cleanup action.
runWidgetInBody :: forall a. Widget a -> Effect (Tuple a (Effect Unit))
runWidgetInBody widget = do
  body <- documentBody
  runWidgetInNode body widget

-- | Runs a widget in the specified parent element and discards cleanup action.
runMainWidgetInNode :: forall a. Node -> Widget a -> Effect a
runMainWidgetInNode parent widget = fst <$> runWidgetInNode parent widget

-- | Runs a widget in `document.body` and discards cleanup action.
runMainWidgetInBody :: forall a. Widget a -> Effect a
runMainWidgetInBody widget = do
  body <- documentBody
  runMainWidgetInNode body widget

-- | Runs a widget in the specified parent element. The widget is destroyed and removed from DOM on cleanup.
spawnWidgetInNode :: forall m a. MonadEffect m => MonadCleanup m => Node -> Widget a -> m a
spawnWidgetInNode node widget = do
  Tuple result cleanup <- liftEffect $ runWidgetInNode node widget
  onCleanup cleanup
  pure result

-- | Runs a widget in `document.body`. The widget is destroyed and removed from DOM on cleanup.
spawnWidgetInBody :: forall m a. MonadEffect m => MonadCleanup m => Widget a -> m a
spawnWidgetInBody widget = do
  body <- liftEffect documentBody
  spawnWidgetInNode body widget

foreign import documentBody :: Effect Node

-- A handy alias for all the constraints you'll need
class (MonadDomBuilder m, MonadFRP m, MonadReplace m, Monoid (m Unit)) <= MonadWidget m

instance monadWidget :: (MonadDomBuilder m, MonadFRP m, MonadReplace m, Monoid (m Unit)) => MonadWidget m

-- | Lift a `Widget` into any `MonadWidget` monad.
liftWidget :: forall m a. MonadDomBuilder m => Widget a -> m a
liftWidget w = let RIO f = unBuilder w in liftBuilder (mkEffectFn1 \env -> runEffectFn1 f (env { userEnv = unit }))

emptyWidget :: Widget Unit
emptyWidget = pure unit
