// new :: forall a. EffectFn1 a (Ref a)
function new_(value) {
  return { value: value };
}

export { new_ as new };

// read :: forall a. EffectFn1 (Ref a) a
export function read(ref) {
  return ref.value;
}

// write :: forall a. EffectFn2 (Ref a) a Unit
export function write(ref, value) {
  ref.value = value;
}
