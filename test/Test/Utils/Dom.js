// dispatchTrivialEvent :: Node -> EventType -> IOSync Unit
exports.dispatchTrivialEvent = function(node) {
  return function(eventType) {
    return function() {
      node.dispatchEvent(new Event(eventType));
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
