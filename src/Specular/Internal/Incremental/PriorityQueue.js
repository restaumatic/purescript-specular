// new :: forall a.
//   EffectFn4
//     (Optional Any)                 -- Optional.none
//     (Field a Mutable Int)          -- Priority. Must be a non-negative integer.
//     (Field a Mutable Boolean)      -- Is the entry present in this queue?
//     (Field a Mutable (Optional a)) -- Next entry with the same priority
//   (PQ a)
exports.new = function(none, priorityField, presentField, nextField) {
  return {
    none: none,

    // Property names of various properties of queue elements.
    priorityField: priorityField,
    presentField: presentField,
    nextField: nextField,

    // Array indexed by priority.
    // Elements within each priority are stored in an intrusive linked list:
    // - `this.none` is the end
    // - `element[this.nextField]` is the next element.
    // `priorityHeads[priority]` contains the head of this list (or `none` if empty).
    priorityHeads: [],

    // Total number of elements in the queue.
    count: 0,

    // TODO: we should track minPriority
  };
};

var PRIORITY_WARNING_MARK = 250;

// add :: forall a. EffectFn2 (PQ a) a Boolean
exports.add = function(pq, node) {
  if(node[pq.presentField]) {
    return false;
  }

  node[pq.presentField] = true;
  pq.count++;

  var priority = node[pq.priorityField];

  while(priority >= pq.priorityHeads.length) {
    pq.priorityHeads.push(pq.none);

    if(pq.priorityHeads.length === PRIORITY_WARNING_MARK) {
      console.warn("Specular: Node height reached " + PRIORITY_WARNING_MARK);
    }
  }

  node[pq.nextField] = pq.priorityHeads[priority];
  pq.priorityHeads[priority] = node;

  return true;
};

var removeMin = function(pq) {
  for(var priority = 0; priority < pq.priorityHeads.length; priority++) {
    var node = pq.priorityHeads[priority];
    if(node !== pq.none) {
      node[pq.presentField] = false;
      pq.priorityHeads[priority] = node[pq.nextField];
      node[pq.nextField] = pq.none;
      pq.count--;
      return node;
    }
  }
  return pq.none;
};

// drain :: forall a. EffectFn2 (PQ a) (EffectFn1 a Unit) Unit
exports.drain = function(pq, fn) {
  while(pq.count > 0) {
    fn(removeMin(pq));
  }
}
