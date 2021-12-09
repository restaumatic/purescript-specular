module Specular.Dom.Widgets.Button
  ( buttonOnClick
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr')
import Specular.Dom.Node.Class (Attrs)
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Event, WeakDynamic)

-- | `buttonOnClick attrs body` - Creates a HTML `<button>` element with the
-- | specified dynamic attributes and body.
-- |
-- | Returns an Event that occurs when the button is clicked.
buttonOnClick :: forall m. MonadWidget m => WeakDynamic Attrs -> m Unit -> m (Event Unit)
buttonOnClick attrs inner = do
  Tuple node _ <- elDynAttr' "button" attrs inner
  domEventWithSample (\_ -> pure unit) "click" node
