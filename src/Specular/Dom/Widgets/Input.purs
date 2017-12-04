module Specular.Dom.Widgets.Input (
    textInputOnChange  
) where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Tuple (Tuple(..))
import Specular.Dom.Browser (Node)
import Specular.Dom.Builder (Builder, domEventWithSample, elDynAttr')
import Specular.Dom.Node.Class (Attrs, (:=))
import Specular.FRP (Dynamic, holdDyn)

textInputOnChange :: String -> Attrs -> Builder Node (Dynamic String)
textInputOnChange initial attrs = do
  Tuple node _ <- elDynAttr' "input" (pure $ attrs <> ("value" := initial)) (pure unit)
  changed <- domEventWithSample (\_ -> getTextInputValue node) "change" node
  holdDyn initial changed

foreign import getTextInputValue :: Node -> IOSync String
