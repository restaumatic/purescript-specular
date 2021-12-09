module Specular.Dom.Widgets.Select
  ( SelectConfig
  , selectInput

  , SelectViewConfig
  , selectView
  ) where

import Prelude

import Effect.Class (liftEffect)
import Data.Array as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int as Int
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Specular.Dom.Builder.Class (domEventWithSample, elAttr, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (getTextInputValue, setTextInputValue)
import Specular.FRP (Dynamic, Event, WeakDynamic, holdDyn)
import Specular.FRP.Base (filterMapEvent)
import Specular.FRP.WeakDynamic (subscribeWeakDyn_)

type SelectConfig a =
  { options :: Array a -- ^ Possible selections
  , initialValueIndex :: Int -- ^ Index of initial value in `options`.
  -- Must be in bounds, else this function will crash
  , display :: a -> String -- ^ Human readable text to display for each option
  , attributes :: WeakDynamic Attrs -- ^ Additional attributes for `<select>`
  }

-- | A <select> element with static list of options.
-- | The returned Dynamic represents the currently selected value.
selectInput
  :: forall m a
   . MonadWidget m
  => SelectConfig a
  -> m (Dynamic a)
selectInput config = do
  let toOption index value = elAttr "option" ("value" := show index) $ text $ config.display value
  Tuple element _ <- elDynAttr' "select" config.attributes $ traverseWithIndex_ toOption config.options
  liftEffect $ setTextInputValue element $ show config.initialValueIndex
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "change" element
  let
    valueChanged = filterMapEvent (Int.fromString >=> Array.index config.options) domChanged

    initialValue = unsafePartial (Array.unsafeIndex config.options config.initialValueIndex)

  holdDyn initialValue valueChanged

type SelectViewConfig a =
  { options :: Array a
  , display :: a -> String
  , attributes :: WeakDynamic Attrs
  }

-- | A <select> element with static list of options.
selectView
  :: forall m a
   . MonadWidget m
  => Eq a
  => SelectViewConfig a
  -> WeakDynamic a -- ^ selected option
  -> m (Event a) -- ^ event: user changed selection
selectView config valueD = do
  let toOption index value = elAttr "option" ("value" := show index) $ text $ config.display value
  Tuple element _ <- elDynAttr' "select" config.attributes $ traverseWithIndex_ toOption config.options

  flip subscribeWeakDyn_ valueD $ \value ->
    for_ (Array.findIndex (eq value) config.options) $ \index ->
      liftEffect $ setTextInputValue element $ show index

  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "change" element
  pure $ filterMapEvent (Int.fromString >=> Array.index config.options) domChanged
