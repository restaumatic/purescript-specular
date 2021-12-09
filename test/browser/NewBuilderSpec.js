// getElementClasses :: String -> Effect (Array ClassName)
exports.getElementClasses = function (selector) {
  return function () {
    return Array.prototype.slice.call(
      document.querySelector(selector).classList
    );
  };
};

exports.clearDocument = function () {
  document.body.innerHTML = "";
};
