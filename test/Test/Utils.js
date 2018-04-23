// getTotalListeners :: forall e. Eff e Int
exports.getTotalListeners = function() {
  return global.__Specular_totalListeners;
};
