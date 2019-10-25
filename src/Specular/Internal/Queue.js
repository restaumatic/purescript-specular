// foreign import new :: forall a. Effect (Queue a)
exports.new = function() {
  return {
    elements: [],
    first: 0,
  };
};

// foreign import enqueue :: forall a. EffectFn2 (Queue a) a Unit
exports.enqueue = function(q, elem) {
  q.elements.push(elem);
};

// foreign import drain :: forall a. EffectFn2 (Queue a) (EffectFn1 a Unit) Unit
exports.drain = function(q, fn) {
  while(q.first < q.elements.length) {
    var elem = q.elements[q.first];
    q.first++;
    fn(elem);
  }
  q.elements = [];
  q.first = 0;
};
