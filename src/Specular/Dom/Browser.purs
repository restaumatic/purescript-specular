module Specular.Dom.Browser where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Specular.Dom.Node.Class (class DOM, class EventDOM, Namespace, TagName, Attrs)

foreign import data Node :: Type

instance domNode :: DOM Node where
  createTextNode = createTextNodeImpl
  setText = setTextImpl
  createDocumentFragment = createDocumentFragmentImpl
  createElementNS (Just namespace) = createElementNSImpl namespace
  createElementNS Nothing = runEffectFn1 createElementImpl

  setAttributes node attrs = runEffectFn2 setAttributesImpl node attrs

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
foreign import createElementImpl :: EffectFn1 TagName Node
foreign import setAttributesImpl :: EffectFn2 Node Attrs Unit
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
