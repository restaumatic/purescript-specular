// getTextInputValue :: Node -> IOSync String
exports.getTextInputValue = function (node) {
  return function () {
    return node.value;
  };
};

// setTextInputValue :: Node -> String -> IOSync String
exports.setTextInputValue = function (node) {
  return function (value) {
    return function () {
      node.value = value;
    };
  };
};

// getCheckboxChecked :: Node -> IOSync Boolean
exports.getCheckboxChecked = function (node) {
  return function () {
    return node.checked;
  };
};

// setCheckboxChecked :: Node -> Boolean -> IOSync Unit
exports.setCheckboxChecked = function (node) {
  return function (value) {
    return function () {
      return (node.checked = value);
    };
  };
};
