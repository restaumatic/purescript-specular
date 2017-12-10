// getTextInputValue :: Node -> IOSync String
exports.getTextInputValue = function(node) {
  return function() {
    return node.value;
  };
};

// getCheckboxChecked :: Node -> IOSync Boolean
exports.getCheckboxChecked = function(node) {
  return function() {
    return node.checked;
  };
};
