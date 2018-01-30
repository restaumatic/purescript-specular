module Specular.Dom.Widget (
    Widget
  , runWidgetInNode 
  , runMainWidgetInNode
  , runMainWidgetInBody

  , class MonadWidget
) where

import Prelude

import Control.Monad.Cleanup (CleanupT, runCleanupT)
import Control.Monad.IOSync (IOSync)
import Control.Monad.IOSync.Class (class MonadIOSync)
import Control.Monad.Replace (class MonadReplace)
import Data.Tuple (Tuple, fst)
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (BuilderT, runBuilderT)
import Specular.Dom.Builder.Class (class MonadDetach, class MonadDomBuilder)
import Specular.FRP (class MonadHold, class MonadHost)

type Widget = BuilderT Node (CleanupT IOSync)

-- | Runs a widget in the specified parent element. Returns the result and cleanup action.
runWidgetInNode :: forall a. Node -> Widget a -> IOSync (Tuple a (IOSync Unit))
runWidgetInNode parent widget = runCleanupT $ runBuilderT {parent} widget

-- | Runs a widget in the specified parent element and discards cleanup action.
runMainWidgetInNode :: forall a. Node -> Widget a -> IOSync a
runMainWidgetInNode parent widget = fst <$> runWidgetInNode parent widget

-- | Runs a widget in `document.body` and discards cleanup action.
runMainWidgetInBody :: forall a. Widget a -> IOSync a
runMainWidgetInBody widget = do
  body <- documentBody
  runMainWidgetInNode body widget

foreign import documentBody :: IOSync Node

-- A handy alias for all the typeclasses you'll need
class (MonadDomBuilder Node m, MonadHost IOSync m, MonadReplace m, MonadHold m, MonadIOSync m, MonadDetach m) <= MonadWidget m
instance monadWidget :: (MonadDomBuilder Node m, MonadHost IOSync m, MonadReplace m, MonadHold m, MonadIOSync m, MonadDetach m) => MonadWidget m
