// data Node :: Type

// createTextNodeImpl :: String -> IOSync Node
export function createTextNodeImpl(text) {
  return function () {
    return document.createTextNode(text);
  };
}

// setTextImpl :: Node -> String -> IOSync Node
export function setTextImpl(node) {
  return function (text) {
    return function () {
      node.textContent = text;
    };
  };
}

// createDocumentFragmentImpl :: IOSync Node
export function createDocumentFragmentImpl() {
  return document.createDocumentFragment();
}

// createElementImpl :: TagName -> IOSync Node
export function createElementImpl(tag) {
  return function () {
    return document.createElement(tag);
  };
}

// createElementNSImpl :: Namespace -> TagName -> IOSync Node
export function createElementNSImpl(namespace) {
  return function (tag) {
    return function () {
      return document.createElementNS(namespace, tag);
    };
  };
}

// _setAttributes :: EffectFn2 Node (Object String) Unit
export function _setAttributes(node, attrs) {
  for (var k in attrs) {
    if (attrs.hasOwnProperty(k)) {
      node.setAttribute(k, attrs[k]);
    }
  }
}

// removeAttributesImpl :: Node -> Array String -> IOSync Unit
export function removeAttributesImpl(node) {
  return function (names) {
    return function () {
      names.forEach(function (name) {
        node.removeAttribute(name);
      });
    };
  };
}

// parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> IOSync (Maybe Node)
export function parentNodeImpl(Just) {
  return function (Nothing) {
    return function (node) {
      return function () {
        var parent = node.parentNode;
        if (parent !== null) {
          return Just(parent);
        } else {
          return Nothing;
        }
      };
    };
  };
}

// insertBeforeImpl :: Node -> Node -> Node -> IOSync Unit
export function insertBeforeImpl(newNode) {
  return function (nodeAfter) {
    return function (parent) {
      return function () {
        parent.insertBefore(newNode, nodeAfter);
      };
    };
  };
}

// appendChildImpl :: Node -> Node -> IOSync Unit
export function appendChildImpl(newNode) {
  return function (parent) {
    return function () {
      parent.appendChild(newNode);
    };
  };
}

// removeAllBetweenImpl :: Node -> Node -> IOSync Unit
export function removeAllBetweenImpl(from) {
  return function (to) {
    return function () {
      if (!from.parentNode) {
        return;
      }
      var node = from.nextSibling;
      while (node && node !== to) {
        var next = node.nextSibling;
        node.parentNode.removeChild(node);
        node = next;
      }
    };
  };
}

// innerHTML :: Node -> IOSync String
export function innerHTML(node) {
  return function () {
    return node.innerHTML;
  };
}

// addEventListenerImpl :: EventType -> (Event -> IOSync Unit) -> Node -> IOSync (IOSync Unit)
export function addEventListenerImpl(eventType) {
  return function (handler) {
    return function (node) {
      return function () {
        var listener = function (event) {
          handler(event)();
        };
        node.addEventListener(eventType, listener);
        return function () {
          node.removeEventListener(eventType, listener);
        };
      };
    };
  };
}

// appendRawHtmlImpl :: String -> Node -> IOSync Unit
export function appendRawHtmlImpl(html) {
  return function (parent) {
    return function () {
      // According to https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML
      // this should work:
      //   parent.insertAdjacentHTML('beforeend', html);
      // But it doesn't, at least in PhantomJS. Hence the following hack:

      // This also should work, but doesn't:
      //   var dummyElement = document.createElement('div');
      //   parent.appendChild(dummyElement);
      //   dummyElement.outerHTML = html;

      var dummyElement = document.createElement("div");
      dummyElement.innerHTML = html;

      var node = dummyElement.firstChild;
      while (node !== null) {
        var next = node.nextSibling;
        parent.appendChild(node); // moves the node from dummyElement to parent
        node = next;
      }
    };
  };
}

// moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> IOSync Unit
export function moveAllBetweenInclusiveImpl(from) {
  return function (to) {
    return function (newParent) {
      return function () {
        var node = from;
        while (true) {
          var next = node.nextSibling;
          newParent.appendChild(node);
          if (node === to) {
            break;
          }
          node = next;
        }
      };
    };
  };
}

// preventDefault :: Event -> IOSync Unit
export function preventDefault(event) {
  return function () {
    return event.preventDefault();
  };
}

// removeNode :: Node -> Effect Unit
export function removeNode(node) {
  return function () {
    if (node.parentNode) {
      node.parentNode.removeChild(node);
    }
  };
}

// getTextInputValue :: Node -> IOSync String
export function getTextInputValue(node) {
  return function () {
    return node.value;
  };
}

// setTextInputValue :: Node -> String -> IOSync String
export function setTextInputValue(node) {
  return function (value) {
    return function () {
      node.value = value;
    };
  };
}

// getCheckboxChecked :: Node -> IOSync Boolean
export function getCheckboxChecked(node) {
  return function () {
    return node.checked;
  };
}

// setCheckboxChecked :: Node -> Boolean -> IOSync Unit
export function setCheckboxChecked(node) {
  return function (value) {
    return function () {
      return (node.checked = value);
    };
  };
}
