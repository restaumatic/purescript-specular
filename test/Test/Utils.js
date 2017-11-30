// dispatchTrivialEvent :: Node -> EventType -> IOSync Unit
exports.dispatchTrivialEvent = function(node) {
  return function(eventType) {
    return function() {
      node.dispatchEvent(new Event(eventType));
    };
  };
};
