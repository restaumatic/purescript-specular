exports.triggerNodeClicked = function (node) {
  return function () {
    return node.click();
  };
};
