// new :: forall a.
//   EffectFn4
//     (Optional Any)                 -- Optional.none
//     (Field a Mutable Int)          -- Priority
//     (Field a Mutable Boolean)      -- Is the entry present in this queue?
//     (Field a Mutable (Optional a)) -- Next entry with the same priority
//   (PQ a)
exports.new = function(none, priorityField, presentField, nextField) {
  return {
    none: none,
    priorityField: priorityField,
    presentField: presentField,
    nextField: nextField,

    priorityHeads: [],
    count: 0,

    // TODO: we should track minPriority
  };
};

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
  }

  node[pq.nextField] = pq.priorityHeads[priority];
  pq.priorityHeads[priority] = node;

  return true;
};

// remove :: forall a. EffectFn2 (PQ a) a Boolean
exports.remove = function(pq, node) {
  if(!node[pq.presentField]) {
    return false;
  }

  node[pq.presentField] = false;

  var priority = node[pq.priorityField];

  if (pq.priorityHeads[priority] === node) {
    pq.priorityHeads[priority] = node[pq.nextField];
  }
  node[pq.nextField] = pq.none;
  pq.count--;

  return true;
};

// isNonEmpty :: forall a. EffectFn1 (PQ a) Boolean
exports.isNonEmpty = function(pq) {
  return pq.count > 0;
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

// removeMin :: forall a. EffectFn1 (PQ a) (Optional a)
exports.removeMin = removeMin;

// drain :: forall a. EffectFn2 (PQ a) (EffectFn1 a Unit) Unit
exports.drain = function(pq, fn) {
  while(pq.count > 0) {
    fn(removeMin(pq));
  }
}
