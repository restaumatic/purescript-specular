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
