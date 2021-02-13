// data Node :: Type

// createTextNodeImpl :: String -> IOSync Node
exports.createTextNodeImpl = function(text) {
  return function() {
    return document.createTextNode(text);
  };
};

// setTextImpl :: Node -> String -> IOSync Node
exports.setTextImpl = function(node) {
  return function(text) {
    return function() {
      node.textContent = text;
    };
  };
};

// createDocumentFragmentImpl :: IOSync Node
exports.createDocumentFragmentImpl = function() {
  return document.createDocumentFragment();
};

// createElementImpl :: TagName -> IOSync Node
exports.createElementImpl = function(tag) {
  return function() {
    return document.createElement(tag);
  };
};

// createElementNSImpl :: Namespace -> TagName -> IOSync Node
exports.createElementNSImpl = function(namespace) {
  return function(tag) {
    return function() {
      return document.createElementNS(namespace, tag);
    };
  };
};

// _setAttributes :: EffectFn2 Node (Object String) Unit
exports._setAttributes = function(node, attrs) {
  for (var k in attrs) {
    if (attrs.hasOwnProperty(k)) {
      node.setAttribute(k, attrs[k]);
    }
  }
};

// removeAttributesImpl :: Node -> Array String -> IOSync Unit
exports.removeAttributesImpl = function(node) {
  return function(names) {
    return function() {
      names.forEach(function(name) {
        node.removeAttribute(name);
      });
    };
  };
};

// parentNodeImpl :: (Node -> Maybe Node) -> Maybe Node -> Node -> IOSync (Maybe Node)
exports.parentNodeImpl = function(Just) {
  return function(Nothing) {
    return function(node) {
      return function() {
        var parent = node.parentNode;
        if(parent !== null) {
          return Just(parent);
        } else {
          return Nothing;
        }
      };
    };
  };
};

// insertBeforeImpl :: Node -> Node -> Node -> IOSync Unit
exports.insertBeforeImpl = function(newNode) {
  return function(nodeAfter) {
    return function(parent) {
      return function() {
        parent.insertBefore(newNode, nodeAfter);
      };
    };
  };
};

// appendChildImpl :: Node -> Node -> IOSync Unit
exports.appendChildImpl = function(newNode) {
  return function(parent) {
    return function() {
      parent.appendChild(newNode);
    };
  };
};

// removeAllBetweenImpl :: Node -> Node -> IOSync Unit
exports.removeAllBetweenImpl = function(from) {
  return function(to) {
    return function() {
      var node = from;
      while(node !== to) {
        var next = node.nextSibling;
        node.parentNode.removeChild(node);
        node = next;
      }
    };
  };
};

// innerHTML :: Node -> IOSync String
exports.innerHTML = function(node) {
  return function() {
    return node.innerHTML;
  };
};

// addEventListenerImpl :: EventType -> (Event -> IOSync Unit) -> Node -> IOSync (IOSync Unit)
exports.addEventListenerImpl = function(eventType) {
  return function(handler) {
    return function(node) {
      return function() {
        var listener = function(event) {
          handler(event)();
        };
        node.addEventListener(eventType, listener);
        return function() {
          node.removeEventListener(eventType, listener);
        };
      };
    };
  };
};

// appendRawHtmlImpl :: String -> Node -> IOSync Unit
exports.appendRawHtmlImpl = function(html) {
  return function(parent) {
    return function() {
      // According to https://developer.mozilla.org/en-US/docs/Web/API/Element/insertAdjacentHTML
      // this should work:
      //   parent.insertAdjacentHTML('beforeend', html);
      // But it doesn't, at least in PhantomJS. Hence the following hack:

      // This also should work, but doesn't:
      //   var dummyElement = document.createElement('div');
      //   parent.appendChild(dummyElement);
      //   dummyElement.outerHTML = html;

      var dummyElement = document.createElement('div');
      dummyElement.innerHTML = html;

      var node = dummyElement.firstChild;
      while(node !== null) {
        var next = node.nextSibling;
        parent.appendChild(node); // moves the node from dummyElement to parent
        node = next;
      }
    };
  };
};

// moveAllBetweenInclusiveImpl :: Node -> Node -> Node -> IOSync Unit
exports.moveAllBetweenInclusiveImpl = function(from) {
  return function(to) {
    return function(newParent) {
      return function() {
        var node = from;
        while(true) {
          var next = node.nextSibling;
          newParent.appendChild(node);
          if(node === to) {
            break;
          }
          node = next;
        }
      };
    };
  };
};

// preventDefault :: Event -> IOSync Unit
exports.preventDefault = function (event) {
  return function() {
    return event.preventDefault();
  }
};

// removeNode :: Node -> Effect Unit
exports.removeNode = function(node) {
  return function() {
    if(node.parentNode) {
      node.parentNode.removeChild(node);
    }
  };
};
