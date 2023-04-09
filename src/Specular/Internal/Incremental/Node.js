export function create(none, dependencies, compute, params) {
  return {
    dependents: [],
    observers: [],
    dependencies,
    compute,
    params,
    value: none,
    height: 0,
    adjustedHeight: 0,
    inRecomputeQueue: false,
    nextInRecomputeQueue: none,
    name: "",

    // For initial `changedAt` we want a value lower than all possible stabilization numbers, but different than `stabilizationIsNotInProgress`.
    // Hence -2.
    changedAt: -2,
  };
}

// foreign import get_dependency :: forall a b. Fn2 (Node a) Int (Node b)
export function get_dependency(node, index) {
  return node.dependencies[index];
}

export function get_dependencies(node) {
  return node.dependencies;
}

export function compute(node) {
  return node.compute(node.params, node);
}

// [[[cog
// immutable_fields = [
//   ['dependents', 'MutableArray SomeNode'],
//   ['observers', 'MutableArray (Observer a)'],
//   ['dependencies', 'MutableArray SomeNode'],
//   ['source', 'Source a'],
// ]
// mutable_fields = [
//   ['adjustedHeight', 'Int'],
//   ['changedAt', 'Int'],
//   ['height', 'Int'],
//   ['name', 'String'],
//   ['value', 'Optional a'],
//   ['params', 'Any'],
// ]
// for name, _ in immutable_fields:
//     cog.outl("""
//     export function get_%(name)s(node) { return node.%(name)s; }
//     """ % {'name': name})
// for name, _ in mutable_fields:
//     cog.outl("""
//     export function get_%(name)s(node) { return node.%(name)s; }
//     export function set_%(name)s(node, value) { node.%(name)s = value; }
//     """ % {'name': name})
// ]]]

export function get_dependents(node) {
  return node.dependents;
}

export function get_observers(node) {
  return node.observers;
}

export function get_adjustedHeight(node) {
  return node.adjustedHeight;
}

export function set_adjustedHeight(node, value) {
  node.adjustedHeight = value;
}

export function get_changedAt(node) {
  return node.changedAt;
}

export function set_changedAt(node, value) {
  node.changedAt = value;
}

export function get_height(node) {
  return node.height;
}

export function set_height(node, value) {
  node.height = value;
}

export function get_name(node) {
  return node.name;
}

export function set_name(node, value) {
  node.name = value;
}

export function get_value(node) {
  return node.value;
}

export function set_value(node, value) {
  node.value = value;
}

// [[[end]]]
