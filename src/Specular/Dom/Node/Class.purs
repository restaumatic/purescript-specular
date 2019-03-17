-- | Module preserved only for backwards compatibility. Use Specular.Dom.Browser instead.
module Specular.Dom.Node.Class (module X) where

import Specular.Dom.Browser (Attrs, Event, EventType, Namespace, Node, TagName, addEventListener, addEventListenerImpl, appendChild, appendChildImpl, appendRawHtml, appendRawHtmlImpl, createDocumentFragment, createDocumentFragmentImpl, createElement, createElementImpl, createElementNS, createElementNSImpl, createTextNode, createTextNodeImpl, innerHTML, insertBefore, insertBeforeImpl, moveAllBetweenInclusive, moveAllBetweenInclusiveImpl, parentNode, parentNodeImpl, preventDefault, removeAllBetween, removeAllBetweenImpl, removeAttributes, removeAttributesImpl, setAttributes, setText, setTextImpl, (:=)) as X
