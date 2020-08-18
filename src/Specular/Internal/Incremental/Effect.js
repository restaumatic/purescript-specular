// foreign import _foreachUntil :: forall a b. EffectFn3 (Optional a) (Array a) (EffectFn1 a (Optional b)) (Optional b)
exports._foreachUntil = function(none, array, fn) {
  for(var i = 0; i < array.length; i++) {
    var result = fn(array[i]);
    if(result !== none) {
      return result;
    }
  }
  return none;
};
