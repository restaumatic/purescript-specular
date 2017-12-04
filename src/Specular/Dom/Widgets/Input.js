// getTextInputValue :: Node -> IOSync String
exports.getTextInputValue = function(node) {
  return function() {
    return node.value;
  };
};
