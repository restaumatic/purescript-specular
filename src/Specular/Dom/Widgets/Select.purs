module Specular.Dom.Widgets.Select
  ( SelectConfig
  , selectInput
  ) where

import Prelude

import Control.Monad.IOSync.Class (liftIOSync)
import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int as Int
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Specular.Dom.Builder.Class (domEventWithSample, elAttr, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, WeakDynamic, holdDyn)
import Specular.FRP.Base (filterMapEvent)

type SelectConfig a =
  { options :: Array a              -- ^ Possible selections
  , initialValueIndex :: Int        -- ^ Index of initial value in `options`.
                                    -- Must be in bounds, else this function will crash
  , display :: a -> String          -- ^ Human readable text to display for each option
  , attributes :: WeakDynamic Attrs -- ^ Additional attributes for `<select>`
  }

-- | A <select> element with static list of options.
-- | The returned Dynamic represents the currently seleted value.
selectInput :: forall m a. MonadWidget m
  => SelectConfig a
  -> m (Dynamic a)
selectInput config = do
  let toOption index value = elAttr "option" ("value" := show index) $ text $ config.display value
  Tuple element _ <- elDynAttr' "select" config.attributes $ traverseWithIndex_ toOption config.options
  liftIOSync $ setTextInputValue element $ show config.initialValueIndex
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "change" element
  let
    valueChanged = filterMapEvent (Int.fromString >=> Array.index config.options) domChanged

    initialValue = unsafePartial (Array.unsafeIndex config.options config.initialValueIndex)

  holdDyn initialValue valueChanged
