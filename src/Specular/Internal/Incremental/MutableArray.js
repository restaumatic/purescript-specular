// empty :: forall a. Effect (MutableArray a)
export function empty() {
  return [];
}

// push :: forall a. EffectFn2 (MutableArray a) a Unit
export function push(self, x) {
  self.push(x);
}

// remove :: forall a. EffectFn2 (MutableArray a) a Unit
export function remove(self, x) {
  var index = self.indexOf(x);

  // The removed element is replaced by the last element of the array.
  // This does not preserve ordering, but is *O*(1). 
  if (index !== -1) {
    var lastIdx = self.length - 1;

    if (index !== lastIdx) {
      self[index] = self[lastIdx];
    }

    self.length = lastIdx;
  }
}

// length :: forall a. EffectFn1 (MutableArray a) Int
export function length(self) {
  return self.length;
}

// iterate :: forall a. EffectFn2 (MutableArray a) (EffectFn1 a Unit) Unit
export function iterate(self, fn) {
  for (let i = 0; i < self.length; i++) {
    fn(self[i]);
  }
}
