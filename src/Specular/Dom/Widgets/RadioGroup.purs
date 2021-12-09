module Specular.Dom.Widgets.RadioGroup
  ( radioGroup
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Partial.Unsafe (unsafePartial)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.Dom.Widgets.Input (BooleanInputType(Radio), booleanInputView)
import Specular.FRP (Dynamic, Event, WeakDynamic, fixFRP, holdDyn, leftmost)
import Specular.FRP.Base (filterMapEvent)

type RadioGroupConfig :: forall k. (k -> Type) -> Type -> Type
type RadioGroupConfig m a =
  { options :: Array a
  -- ^ Possible selections
  , initialValueIndex :: Int
  -- ^ Index of initial value in `options`.
  -- Must be in bounds, else `radioGroup` will crash
  , render :: forall b. String -> a -> (WeakDynamic Attrs -> m b) -> m b
  -- ^ Render an option. Takes the radio input ID, the value and the radio input.
  -- Must return the return value of the radio, as evidenced by the type.
  --
  -- The radio input ID is intended to be passed to the `for` attribute of
  -- `<label>`. If you do that, click events on label cause the radio to be selected.
  }

radioGroup
  :: forall m a
   . MonadWidget m
  => RadioGroupConfig m a
  -> m (Dynamic a)
radioGroup config = fixFRP $ \selectedIndex -> do
  let randomIdentifier = liftEffect $ map (\n -> "radio" <> show n) random
  name <- randomIdentifier
  -- ^ FIXME: document this sorcery

  (changeEvents :: Array (Event (Tuple Int a))) <-
    forWithIndex config.options $ \index option -> do
      id <- randomIdentifier
      let
        isSelected = map (_ == index) selectedIndex
        radio extraAttrs =
          let
            attrs = map (\x -> "name" := name <> "id" := id <> x) extraAttrs
          in
            map
              ( filterMapEvent
                  ( \b ->
                      if b then Just (Tuple index option)
                      else Nothing
                  )
              ) $
              booleanInputView Radio isSelected attrs
      config.render id option radio

  let
    initialValue = unsafePartial (Array.unsafeIndex config.options config.initialValueIndex)
    valueChanged = leftmost changeEvents

  (value :: Dynamic (Tuple Int a)) <-
    holdDyn (Tuple config.initialValueIndex initialValue) valueChanged
  pure $ Tuple (map fst value) (map snd value)
