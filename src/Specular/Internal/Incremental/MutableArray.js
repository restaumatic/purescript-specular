// empty :: forall a. Effect (MutableArray a)
exports.empty = function() {
  return [];
};

// push :: forall a. EffectFn2 (MutableArray a) a Unit
exports.push = function(self, x) {
  self.push(x);
};

// remove :: forall a. EffectFn2 (MutableArray a) a Unit
exports.remove = function(self, x) {
  var index = self.indexOf(x);
  if(index !== -1) {
    self.splice(index, 1);
  }
};

// length :: forall a. EffectFn1 (MutableArray a) Int
exports.length = function(self) {
  return self.length;
};

// iterate :: forall a. EffectFn2 (MutableArray a) (EffectFn1 a Unit) Unit
exports.iterate = function(self, fn) {
  for(var i = 0; i < self.length; i++) {
    fn(self[i]);
  }
};
