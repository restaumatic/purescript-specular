// foreachEFn :: forall a. EffectFn2 (Array a) (EffectFn1 a Unit) Unit
exports.foreachEFn = function(xs, fn) {
  for(var i = 0; i < xs.length; i++) {
    fn(xs[i]);
  }
};
