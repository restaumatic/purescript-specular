module Specular.Dom.Widgets.Input (
    textInputOnChange  
  , textInputOnInput
) where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder.Class (class MonadWidget, domEventWithSample, elDynAttr')
import Specular.Dom.Node.Class (Attrs, (:=))
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

foreign import getTextInputValue :: Node -> IOSync String
