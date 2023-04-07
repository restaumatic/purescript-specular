// iterate :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
export function iterate(self, fn) {
  for (var i = 0; i < self.length; i++) {
    fn(self[i]);
  }
}

export function mapE(self, fn) {
  return self.map(fn);
}
