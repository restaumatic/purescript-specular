// sequenceEffects :: Array Effect -> Effect
exports.sequenceEffects = function(effects) {
  return function seuqenceEffects_eff() {
    for(var i = 0; i < effects.length; i++) {
      effects[i]();
    }
  };
};
