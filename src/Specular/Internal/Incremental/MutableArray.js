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
  if (index !== -1) {
    self[index] = null;
  }
}

// length :: forall a. EffectFn1 (MutableArray a) Int
export function length(self) {
  return self.length;
}

// iterate :: forall a. EffectFn2 (MutableArray a) (EffectFn1 a Unit) Unit
export function iterate(self, fn) {
  var nullIndices = [];
  for (var i = 0; i < self.length; i++) {
    if (self[i] === null) {
      nullIndices.push(i);
    } else {
      fn(self[i]);
    }
  }

  for (var i = nullIndices.length - 1; i >= 0; i--) {
    self.splice(nullIndices[i], 1);
  }
}
