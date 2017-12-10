module Specular.Dom.Widgets.Input (
    textInputOnChange  
  , textInputOnInput
  , textareaOnChange
  , checkbox
) where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (domEventWithSample, elDynAttr', text)
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.Dom.Widget (class MonadWidget)
import Specular.FRP (Dynamic, holdDyn)

textInputOnChange :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textInputOnChange initial attrs = do
  Tuple node _ <- elDynAttr' "input" (pure $ attrs <> ("value" := initial)) (pure unit)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  holdDyn initial changed

textInputOnInput :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textInputOnInput initial attrs = do
  Tuple node _ <- elDynAttr' "input" (pure $ attrs <> ("value" := initial)) (pure unit)
  input <- domEventWithSample (\_ -> getTextInputValue node) "input" node
  holdDyn initial input

textareaOnChange :: forall m. MonadWidget m => String -> Attrs -> m (Dynamic String)
textareaOnChange initial attrs = do
  Tuple node _ <- elDynAttr' "textarea" (pure attrs) (text initial)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  holdDyn initial changed

foreign import getTextInputValue :: Node -> IOSync String

checkbox :: forall m. MonadWidget m
  => Boolean -- ^ initial value
  -> Attrs
  -> m (Dynamic Boolean)
checkbox initial attrs = do
  let attrs' = attrs <> ("type" := "checkbox") <> (if initial then "checked" := "checked" else mempty)
  Tuple node _ <- elDynAttr' "input" (pure attrs') (pure unit)
  changed <- domEventWithSample (\_ -> getCheckboxChecked node) "change" node
  holdDyn initial changed

foreign import getCheckboxChecked :: Node -> IOSync Boolean
