// iterate :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
exports.iterate = function(self, fn) {
  for(var i = 0; i < self.length; i++) {
    fn(self[i]);
  }
};
