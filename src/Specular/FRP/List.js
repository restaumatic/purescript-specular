// nextMicrotask :: Effect Unit -> Effect Unit
export function nextMicrotask(eff) {
  return function () {
    Promise.resolve().then(eff);
  };
}
