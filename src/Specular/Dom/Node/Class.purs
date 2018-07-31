module Specular.Dom.Node.Class where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

type Attrs = Object String

-- | Convenient syntax for building Attrs
infix 8 Object.singleton as :=

type TagName = String

-- | XML namespace URI.
type Namespace = String

class DOM node where
  createTextNode :: String -> Effect node
  setText :: node -> String -> Effect Unit

  createDocumentFragment :: Effect node

  -- | Create an element, optionally with namespace.
  createElementNS :: Maybe Namespace -> TagName -> Effect node

  setAttributes :: node -> Attrs -> Effect Unit
  removeAttributes :: node -> Array String -> Effect Unit

  -- | Return parent node of the node,
  -- | or Nothing if it has been detached.
  parentNode :: node -> Effect (Maybe node)

  -- | `insertBefore newNode nodeAfter parent`
  -- | Insert `newNode` before `nodeAfter` in `parent`
  insertBefore :: node -> node -> node -> Effect Unit

  -- | `appendChild newNode parent`
  appendChild :: node -> node -> Effect Unit

  -- | Append a chunk of raw HTML to the end of the node.
  appendRawHtml :: String -> node -> Effect Unit

  -- | `removeAllBetween from to`
  -- |
  -- | Remove `from` and all nodes after `from` and before `to` from their
  -- | parent. `to` is not removed.
  -- |
  -- | Assumes that `from` and `to` have the same parent,
  -- | and `from` is before `to`.
  removeAllBetween :: node -> node -> Effect Unit

  -- | `moveAllBetweenInclusive from to parent`
  -- |
  -- | Moves `from`, all nodes after `from` and before `to` and `to` to
  -- | `parent`.
  -- |
  -- | Assumes that `from` and `to` have the same parent,
  -- | and `from` is before `to`.
  moveAllBetweenInclusive :: node -> node -> node -> Effect Unit

  childNodes :: node -> Effect (Array node)

type EventType = String

class DOM node <= EventDOM event node | node -> event where
  -- | Register an event listener. Returns unregister action.
  addEventListener :: EventType -> (event -> Effect Unit) -> node -> Effect (Effect Unit)

createElement :: forall node. DOM node => TagName -> Effect node
createElement = createElementNS Nothing
