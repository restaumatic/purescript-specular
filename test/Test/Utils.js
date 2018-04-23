// getTotalListeners :: forall e. Eff e Int
exports.getTotalListeners = function() {
  return global.__Specular_totalListeners;
};

// modifyTotalListeners :: forall e. (Int -> Int) -> Eff e Unit
exports.modifyTotalListeners = function(f) {
  return function() {
    global.__Specular_totalListeners = f(global.__Specular_totalListeners);
  };
}
