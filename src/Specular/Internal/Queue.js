// foreign import new :: forall a. Effect (Queue a)
function new_() {
  return {
    elements: [],
    end: 0,
    first: 0,
  };
}

export { new_ as new };

// foreign import enqueue :: forall a. EffectFn2 (Queue a) a Unit
export function enqueue(q, elem) {
  q.elements[q.end] = elem;
  q.end++;
}

// foreign import drain :: forall a. EffectFn2 (Queue a) (EffectFn1 a Unit) Unit
export function drain(q, fn) {
  while (q.first < q.end) {
    var elem = q.elements[q.first];
    q.elements[q.first] = undefined;
    q.first++;
    fn(elem);
  }
  q.first = 0;
  q.end = 0;
}
