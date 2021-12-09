// foreign import new :: forall a. Effect (Queue a)
exports.new = function () {
  return {
    elements: [],
    end: 0,
    first: 0,
  };
};

// foreign import enqueue :: forall a. EffectFn2 (Queue a) a Unit
exports.enqueue = function (q, elem) {
  q.elements[q.end] = elem;
  q.end++;
};

// foreign import drain :: forall a. EffectFn2 (Queue a) (EffectFn1 a Unit) Unit
exports.drain = function (q, fn) {
  while (q.first < q.end) {
    var elem = q.elements[q.first];
    q.elements[q.first] = undefined;
    q.first++;
    fn(elem);
  }
  q.first = 0;
  q.end = 0;
};
