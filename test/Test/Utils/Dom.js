// dispatchEvent :: Node -> EventType -> Foreign -> Effect Unit
export function dispatchEvent(node) {
  return function (eventType) {
    return function (options) {
      return function () {
        node.dispatchEvent(new Event(eventType, options));
      };
    };
  };
}

// querySelector :: String -> Node -> Effect Node
export function querySelector(selector) {
  return function (parent) {
    return function () {
      var node = parent.querySelector(selector);
      if (!node) {
        throw new Error("Node not found for selector: " + selector);
      }
      return node;
    };
  };
}

// setInputValueWithChange :: String -> Node -> Effect Unit
export function setInputValueWithChange(value) {
  return function (node) {
    return function () {
      node.value = value;
      node.dispatchEvent(new Event("change"));
    };
  };
}

// numChildNodes :: Node -> Effect Int
export function numChildNodes(node) {
  return function () {
    return node.childNodes.length;
  };
}
