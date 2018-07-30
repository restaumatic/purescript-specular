module Specular.Dom.Browser
  ( Node
  , innerHTML

  , Event
  , preventDefault
  ) where

import Prelude

import Effect (Effect)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Data.Tuple (Tuple(..))
import Specular.Dom.Node.Class (class DOM, class EventDOM, TagName, Namespace)

foreign import data Node :: Type

instance domNode :: DOM Node where
  createTextNode = createTextNodeImpl
  setText = setTextImpl
  createDocumentFragment = createDocumentFragmentImpl
  createElementNS (Just namespace) = createElementNSImpl namespace
  createElementNS Nothing = createElementImpl

  setAttributes node attrs =
    for_ (Object.toUnfoldable attrs :: Array (Tuple String String)) $ \(Tuple name value) ->
      setAttributeImpl node name value

  removeAttributes = removeAttributesImpl

  parentNode = parentNodeImpl Just Nothing
  insertBefore = insertBeforeImpl
  appendChild = appendChildImpl
  removeAllBetween = removeAllBetweenImpl

  appendRawHtml = appendRawHtmlImpl

  childNodes = childNodesImpl

  moveAllBetweenInclusive = moveAllBetweenInclusiveImpl

foreign import createTextNodeImpl :: String -> Effect Node
foreign import setTextImpl :: Node -> String -> Effect Unit
foreign import createDocumentFragmentImpl :: Effect Node
foreign import createElementNSImpl :: Namespace -> TagName -> Effect Node
foreign import createElementImpl :: TagName -> Effect Node
foreign import setAttributeImpl :: Node -> String -> String -> Effect Unit
foreign import removeAttributesImpl :: Node -> Array String -> Effect Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> Effect (Maybe Node)
foreign import insertBeforeImpl :: Node -> Node -> Node -> Effect Unit
foreign import appendChildImpl :: Node -> Node -> Effect Unit
foreign import removeAllBetweenImpl :: Node -> Node -> Effect Unit
foreign import appendRawHtmlImpl :: String -> Node -> Effect Unit
foreign import childNodesImpl :: Node -> Effect (Array Node)
foreign import moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> Effect Unit

foreign import data Event :: Type

instance eventDomNode :: EventDOM Event Node where
  addEventListener = addEventListenerImpl

foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)

-- | JS `Event.preventDefault()`.
foreign import preventDefault :: Event -> Effect Unit

-- | Get `innerHTML` of a node.
foreign import innerHTML :: Node -> Effect String
