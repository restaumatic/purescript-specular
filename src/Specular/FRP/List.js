// nextMicrotask :: Effect Unit -> Effect Unit
exports.nextMicrotask = function(eff) {
  return function() {
    Promise.resolve().then(eff);
  };
};
