module Specular.Dom.Browser where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Specular.Dom.Node.Class (class DOM, class EventDOM, TagName)

foreign import data Node :: Type

instance domNode :: DOM Node where
  createTextNode = createTextNodeImpl
  createDocumentFragment = createDocumentFragmentImpl
  createElement = createElementImpl

  setAttributes node attrs =
    for_ (SM.toUnfoldable attrs :: Array (Tuple String String)) $ \(Tuple name value) ->
      setAttributeImpl node name value

  removeAttributes = removeAttributesImpl

  parentNode = parentNodeImpl Just Nothing
  insertBefore = insertBeforeImpl
  appendChild = appendChildImpl
  removeAllBetween = removeAllBetweenImpl

foreign import createTextNodeImpl :: String -> IOSync Node
foreign import createDocumentFragmentImpl :: IOSync Node
foreign import createElementImpl :: TagName -> IOSync Node
foreign import setAttributeImpl :: Node -> String -> String -> IOSync Unit
foreign import removeAttributesImpl :: Node -> Array String -> IOSync Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> IOSync (Maybe Node)
foreign import insertBeforeImpl :: Node -> Node -> Node -> IOSync Unit
foreign import appendChildImpl :: Node -> Node -> IOSync Unit
foreign import removeAllBetweenImpl :: Node -> Node -> IOSync Unit

foreign import innerHTML :: Node -> IOSync String

foreign import data Event :: Type

instance eventDomNode :: EventDOM Event Node where
  addEventListener = addEventListenerImpl

foreign import addEventListenerImpl :: String -> (Event -> IOSync Unit) -> Node -> IOSync (IOSync Unit)
