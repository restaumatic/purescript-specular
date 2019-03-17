// data Ref :: Type -> Type

// _newRef :: forall a. EffectFn1 a (Ref a)
exports._newRef = function(initial) {
  return {
    value: initial
  };
};

// _readRef :: forall a. EffectFn1 (Ref a) a
exports._readRef = function(ref) {
  return ref.value;
};

// _writeRef :: forall a. EffectFn2 (Ref a) a Unit
exports._writeRef = function(ref, newValue) {
  ref.value = newValue;
};

// data DelayedEffects :: Type

// emptyDelayed :: Effect DelayedEffects
exports.emptyDelayed = function() {
  return [];
};

// _pushDelayed :: EffectFn2 DelayedEffects (Effect Unit) Unit
exports._pushDelayed = function(effs, eff) {
  effs.push(eff);
};

// _unsafeFreezeDelayed :: EffectFn1 DelayedEffects (Array (Effect Unit))
exports._unsafeFreezeDelayed = function(x) {
  return x;
};

// _sequenceEffects :: EffectFn1 (Array (Effect Unit)) Unit
exports._sequenceEffects = function(effects) {
  for(var i = 0; i < effects.length; i++) {
    effects[i]();
  }
};
