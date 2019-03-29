module Specular.Dom.Widgets.Input (
    textInputOnChange  
  , textInputOnInput
  , textareaOnChange

  , TextInput
  , TextInputConfig
  , textInput
  , textInputValue
  , textInputValueOnChange
  , textInputValueEventOnEnter

  , checkbox
  , checkboxView
  , BooleanInputType(..)
  , booleanInputView
  
  -- TODO: move to Internal
  , getTextInputValue
  , setTextInputValue
) where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Browser as Browser
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (class MonadFRP, Dynamic, Event, WeakDynamic, filterEvent, holdDyn, leftmost, never, subscribeWeakDyn_)
import Specular.FRP.Base (subscribeEvent_, tagDyn)
import Unsafe.Coerce (unsafeCoerce)

textInputOnChange :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textInputOnChange initial attrs = do
  input <- textInput { initialValue: initial, attributes: pure attrs, setValue: never }
  textInputValueOnChange input

textInputOnInput :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textInputOnInput initial attrs = do
  textInputValue <$> textInput { initialValue: initial, attributes: pure attrs, setValue: never }

textareaOnChange :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textareaOnChange initial attrs = do
  Tuple node _ <- elDynAttr' "textarea" (pure attrs) (text initial)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  holdDyn initial changed

checkbox :: forall m. MonadWidget m
  => Boolean -- ^ initial value
  -> Attrs
  -> m (Dynamic Boolean)
checkbox initial attrs = do
  let attrs' = attrs <> ("type" := "checkbox") <> (if initial then "checked" := "checked" else mempty)
  Tuple node _ <- elDynAttr' "input" (pure attrs') (pure unit)
  changed <- domEventWithSample (\_ -> getCheckboxChecked node) "change" node
  holdDyn initial changed

checkboxView :: forall m. MonadWidget m
  => WeakDynamic Boolean
  -> WeakDynamic Attrs
  -> m (Event Boolean)
checkboxView = booleanInputView Checkbox

data BooleanInputType = Checkbox | Radio

booleanInputTypeToAttributeValue :: BooleanInputType -> String
booleanInputTypeToAttributeValue =
  case _ of
    Checkbox -> "checkbox"
    Radio    -> "radio"

booleanInputView :: forall m. MonadWidget m
  => BooleanInputType
  -> WeakDynamic Boolean
  -> WeakDynamic Attrs
  -> m (Event Boolean)
booleanInputView type_ dchecked dattrs = do
  let dattrs' = lift2 (\attrs checked -> attrs <>
                        ("type" := booleanInputTypeToAttributeValue type_) <>
                        (if checked then "checked" := "checked" else mempty)
                     ) dattrs dchecked

  Tuple node _ <- elDynAttr' "input" dattrs' (pure unit)
  subscribeWeakDyn_ (setCheckboxChecked node) dchecked
  domEventWithSample (\_ -> getCheckboxChecked node) "change" node

foreign import getCheckboxChecked :: Node -> Effect Boolean
foreign import setCheckboxChecked :: Node -> Boolean -> Effect Unit

type TextInputConfig =
  { initialValue :: String
  , setValue :: Event String
  , attributes :: WeakDynamic Attrs
  }

newtype TextInput = TextInput
  { element :: Node
  , value :: Dynamic String
  }

textInput :: forall m. MonadWidget m
  => TextInputConfig
  -> m TextInput
textInput config = do
  Tuple element _ <- elDynAttr' "input" config.attributes (pure unit)
  liftEffect $ setTextInputValue element config.initialValue
  subscribeEvent_ (setTextInputValue element) config.setValue
  domChanged <- domEventWithSample (\_ -> getTextInputValue element) "input" element
  value <- holdDyn config.initialValue (leftmost [ domChanged, config.setValue ])
  pure $ TextInput { element, value }

textInputValue :: TextInput -> Dynamic String
textInputValue (TextInput {value}) = value

textInputValueOnChange :: forall m. MonadFRP m
  => TextInput
  -> m (Dynamic String)
textInputValueOnChange (TextInput {element}) = do
  initial <- liftEffect $ getTextInputValue element
  changed <- domEventWithSample (\_ -> getTextInputValue element) "change" element
  holdDyn initial changed

textInputValueEventOnEnter :: forall m. MonadFRP m
  => TextInput
  -> m (Event String)
textInputValueEventOnEnter (TextInput {element,value}) = do
  keypress <- domEventWithSample unsafeEventKey "keypress" element
  let enter = void $ filterEvent (_ == "Enter") keypress
  pure $ tagDyn value enter

unsafeEventKey :: Browser.Event -> Effect String
unsafeEventKey event = pure (unsafeCoerce event).key

foreign import getTextInputValue :: Node -> Effect String
foreign import setTextInputValue :: Node -> String -> Effect Unit
