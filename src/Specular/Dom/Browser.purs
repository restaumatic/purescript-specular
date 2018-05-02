module Specular.Dom.Browser
  ( Node
  , innerHTML

  , Event
  , preventDefault
  ) where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
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
    for_ (SM.toUnfoldable attrs :: Array (Tuple String String)) $ \(Tuple name value) ->
      setAttributeImpl node name value

  removeAttributes = removeAttributesImpl

  parentNode = parentNodeImpl Just Nothing
  insertBefore = insertBeforeImpl
  appendChild = appendChildImpl
  removeAllBetween = removeAllBetweenImpl

  appendRawHtml = appendRawHtmlImpl

  childNodes = childNodesImpl

  moveAllBetweenInclusive = moveAllBetweenInclusiveImpl

foreign import createTextNodeImpl :: String -> IOSync Node
foreign import setTextImpl :: Node -> String -> IOSync Unit
foreign import createDocumentFragmentImpl :: IOSync Node
foreign import createElementNSImpl :: Namespace -> TagName -> IOSync Node
foreign import createElementImpl :: TagName -> IOSync Node
foreign import setAttributeImpl :: Node -> String -> String -> IOSync Unit
foreign import removeAttributesImpl :: Node -> Array String -> IOSync Unit
foreign import parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> IOSync (Maybe Node)
foreign import insertBeforeImpl :: Node -> Node -> Node -> IOSync Unit
foreign import appendChildImpl :: Node -> Node -> IOSync Unit
foreign import removeAllBetweenImpl :: Node -> Node -> IOSync Unit
foreign import appendRawHtmlImpl :: String -> Node -> IOSync Unit
foreign import childNodesImpl :: Node -> IOSync (Array Node)
foreign import moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> IOSync Unit

foreign import data Event :: Type

instance eventDomNode :: EventDOM Event Node where
  addEventListener = addEventListenerImpl

foreign import addEventListenerImpl :: String -> (Event -> IOSync Unit) -> Node -> IOSync (IOSync Unit)

-- | JS `Event.preventDefault()`.
foreign import preventDefault :: Event -> IOSync Unit

-- | Get `innerHTML` of a node.
foreign import innerHTML :: Node -> IOSync String
