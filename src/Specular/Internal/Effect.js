// data DelayedEffects :: Type

// emptyDelayed :: Effect DelayedEffects
export function emptyDelayed() {
  return [];
}

// pushDelayed :: DelayedEffects -> Effect Unit -> Effect Unit
export function pushDelayed(effs) {
  return function (eff) {
    return function () {
      effs.push(eff);
    };
  };
}

// unsafeFreezeDelayed :: DelayedEffects -> Effect (Array (Effect Unit))
export function unsafeFreezeDelayed(x) {
  return function () {
    return x;
  };
}

// sequenceEffects :: Array (Effect Unit) -> Effect Unit
export function sequenceEffects(effects) {
  return function sequenceEffects_eff() {
    for (var i = 0; i < effects.length; i++) {
      effects[i]();
    }
  };
}
