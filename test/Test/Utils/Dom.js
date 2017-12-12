// dispatchEvent :: Node -> EventType -> Foreign -> IOSync Unit
exports.dispatchEvent = function(node) {
  return function(eventType) {
    return function(options) {
      return function() {
        node.dispatchEvent(new Event(eventType, options));
      };
    };
  };
};

// querySelector :: String -> Node -> IOSync Node
exports.querySelector = function(selector) {
  return function(parent) {
    return function() {
      var node = parent.querySelector(selector);
      if(!node) {
        throw new Error('Node not found for selector: ' + selector);
      }
      return node;
    };
  };
};

// setInputValueWithChange :: String -> Node -> IOSync Unit
exports.setInputValueWithChange = function(value) {
  return function(node) {
    return function() {
      node.value = value;
      node.dispatchEvent(new Event('change'));
    };
  };
};
