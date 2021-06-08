// data DelayedEffects :: Type

// emptyDelayed :: Effect DelayedEffects
exports.emptyDelayed = function() {
  return [];
};

// pushDelayed :: DelayedEffects -> Effect Unit -> Effect Unit
exports.pushDelayed = function(effs) {
  return function(eff) {
    return function() {
      effs.push(eff);
    };
  };
};

// unsafeFreezeDelayed :: DelayedEffects -> Effect (Array (Effect Unit))
exports.unsafeFreezeDelayed = function(x) {
  return function() {
    return x;
  };
};

// sequenceEffects :: Array (Effect Unit) -> Effect Unit
exports.sequenceEffects = function(effects) {
  return function sequenceEffects_eff() {
    for(var i = 0; i < effects.length; i++) {
      effects[i]();
    }
  };
};
