module Specular.Dom.Node.Class where

import Prelude

import Control.Monad.IOSync (IOSync)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)

type Attrs = StrMap String

type TagName = String

class DOM node where
  createTextNode :: String -> IOSync node
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

  -- | `removeAllBetween from to`
  -- |
  -- | Remove `from` and all nodes after `from` and before `to` from their
  -- | parent. `to` is not removed.
  -- |
  -- | Assumes that `from` and `to` have the same parent,
  -- | and `from` is before `to`.
  removeAllBetween :: node -> node -> IOSync Unit
