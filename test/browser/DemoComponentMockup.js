export function setHash(hash) {
  return function () {
    return window.location.hash = "#" + hash;
  }
}

// onHash :: (String -> Effect Unit) -> Effect Unit
export function onHash(handler) {
  return function () {
    function update() {
      handler(window.location.hash.substring(1))();
    }
    update();
    window.addEventListener("hashchange", update);
  };
}

export function setTitle(title) {
  return function () {
    document.title = title;
  }
}

export function onKeyDown(handler) {
  return function () {
    function update(event) {
      handler(event.code)();
    }
    // Add event listener on keydown
    document.addEventListener('keydown', update);
  }
}