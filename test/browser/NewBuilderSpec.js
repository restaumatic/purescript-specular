// getElementClasses :: String -> Effect (Array ClassName)
export function getElementClasses(selector) {
  return function () {
    return Array.prototype.slice.call(
      document.querySelector(selector).classList
    );
  };
}

export function clearDocument() {
  document.body.innerHTML = "";
}

// getElementProperty :: String -> String -> Effect a
export function getElementProperty(selector) {
  return function (property) {
    return function () {
      return document.querySelector(selector)[property];
    };
  };
}
