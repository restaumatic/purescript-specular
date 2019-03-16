module Specular.Dom.Browser where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object

type Attrs = Object String

-- | Convenient syntax for building Attrs
infix 8 Object.singleton as :=

type TagName = String

-- | XML namespace URI.
type Namespace = String

-- | DOM node.
foreign import data Node :: Type

-- | DOM event.
foreign import data Event :: Type

-- | HTML event type, e.g. "click".
type EventType = String

-- | Register an event listener. Returns unregister action.
addEventListener :: EventType -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)
addEventListener = addEventListenerImpl

createTextNode :: String -> Effect Node
createTextNode = createTextNodeImpl

setText :: Node -> String -> Effect Unit
setText = setTextImpl

createDocumentFragment :: Effect Node
createDocumentFragment = createDocumentFragmentImpl

-- | Create an element, optionally with namespace.
createElementNS :: Maybe Namespace -> TagName -> Effect Node
createElementNS (Just namespace) = createElementNSImpl namespace
createElementNS Nothing = createElementImpl

createElement :: TagName -> Effect Node
createElement = createElementNS Nothing

setAttributes :: Node -> Attrs -> Effect Unit
setAttributes node attrs =
  for_ (Object.toUnfoldable attrs :: Array (Tuple String String)) $ \(Tuple name value) ->
    setAttributeImpl node name value

removeAttributes :: Node -> Array String -> Effect Unit
removeAttributes = removeAttributesImpl

-- | Return parent node of the node,
-- | or Nothing if it has been detached.
parentNode :: Node -> Effect (Maybe Node)
parentNode = parentNodeImpl Just Nothing

-- | `insertBefore newNode nodeAfter parent`
-- | Insert `newNode` before `nodeAfter` in `parent`
insertBefore :: Node -> Node -> Node -> Effect Unit
insertBefore = insertBeforeImpl

-- | `appendChild newNode parent`
appendChild :: Node -> Node -> Effect Unit
appendChild = appendChildImpl

-- | Append a chunk of raw HTML to the end of the node.
appendRawHtml :: String -> Node -> Effect Unit
appendRawHtml = appendRawHtmlImpl

-- | `removeAllBetween from to`
-- |
-- | Remove `from` and all nodes after `from` and before `to` from their
-- | parent. `to` is not removed.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.
removeAllBetween :: Node -> Node -> Effect Unit
removeAllBetween = removeAllBetweenImpl

-- | `moveAllBetweenInclusive from to parent`
-- |
-- | Moves `from`, all nodes after `from` and before `to` and `to` to
-- | `parent`.
-- |
-- | Assumes that `from` and `to` have the same parent,
-- | and `from` is before `to`.
moveAllBetweenInclusive :: Node -> Node -> Node -> Effect Unit
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
foreign import moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> Effect Unit

foreign import addEventListenerImpl :: String -> (Event -> Effect Unit) -> Node -> Effect (Effect Unit)

-- | JS `Event.preventDefault()`.
foreign import preventDefault :: Event -> Effect Unit

-- | Get `innerHTML` of a node.
foreign import innerHTML :: Node -> Effect String
