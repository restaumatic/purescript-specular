// data Ref :: Type -> Type

// newRef :: forall a. a -> Effect (Ref a)
exports._newRef = function(initial) {
  return {
    value: initial
  };
};

// readRef :: forall a. Ref a -> Effect a
exports._readRef = function(ref) {
  return ref.value;
};

// writeRef :: forall a. Ref a -> a -> Effect Unit
exports._writeRef = function(ref, newValue) {
  ref.value = newValue;
};

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
