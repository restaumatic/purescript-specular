module Specular.Dom.Node.Class where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap

type Attrs = StrMap String

-- | Convenient syntax for building Attrs
infix 8 StrMap.singleton as :=

type TagName = String

class DOM node where
  createTextNode :: String -> IOSync node
  setText :: node -> String -> IOSync Unit

  createDocumentFragment :: IOSync node
  createElement :: TagName -> IOSync node

  setAttributes :: node -> Attrs -> IOSync Unit
  removeAttributes :: node -> Array String -> IOSync Unit

  -- | Return parent node of the node,
  -- | or Nothing if it has been detached.
  parentNode :: node -> IOSync (Maybe node)

  -- | `insertBefore newNode nodeAfter parent`
  -- | Insert `newNode` before `nodeAfter` in `parent`
  insertBefore :: node -> node -> node -> IOSync Unit

  -- | `appendChild newNode parent`
  appendChild :: node -> node -> IOSync Unit

  -- | Append a chunk of raw HTML to the end of the node.
  appendRawHtml :: String -> node -> IOSync Unit

  -- | `removeAllBetween from to`
  -- |
  -- | Remove `from` and all nodes after `from` and before `to` from their
  -- | parent. `to` is not removed.
  -- |
  -- | Assumes that `from` and `to` have the same parent,
  -- | and `from` is before `to`.
  removeAllBetween :: node -> node -> IOSync Unit

  -- | `moveAllBetweenInclusive from to parent`
  -- |
  -- | Moves `from`, all nodes after `from` and before `to` and `to` to
  -- | `parent`.
  -- |
  -- | Assumes that `from` and `to` have the same parent,
  -- | and `from` is before `to`.
  moveAllBetweenInclusive :: node -> node -> node -> IOSync Unit

  childNodes :: node -> IOSync (Array node)

type EventType = String

class DOM node <= EventDOM event node | node -> event where
  -- | Register an event listener. Returns unregister action.
  addEventListener :: EventType -> (event -> IOSync Unit) -> node -> IOSync (IOSync Unit)
