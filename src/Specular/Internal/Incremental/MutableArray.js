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
  let writeIndex = 0;

  // Clean up array using in-place filtering technique
  for (let i = 0; i < self.length; i++) {
    const value = self[i];
    if (value !== null) {
      fn(value);
      if (writeIndex !== i) {
        self[writeIndex] = value; // Move non-null values to the left
      }
      writeIndex++;
    }
  }

  // Trim the array to remove null values
  self.length = writeIndex;
}
