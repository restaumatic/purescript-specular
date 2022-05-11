export function triggerNodeClicked(node) {
  return function () {
    return node.click();
  };
}
