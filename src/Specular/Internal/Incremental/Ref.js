// new :: forall a. EffectFn1 a (Ref a)
exports.new = function(value) {
  return { value: value };
};

// read :: forall a. EffectFn1 (Ref a) a
exports.read = function(ref) {
  return ref.value;
};

// write :: forall a. EffectFn2 (Ref a) a Unit
exports.write = function(ref, value) {
  ref.value = value;
};
