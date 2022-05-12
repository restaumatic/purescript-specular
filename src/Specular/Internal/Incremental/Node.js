export function _new(none, source, dependents, observers, value, height) {
  return {
    source: source,
    dependents: dependents,
    observers: observers,
    value: value,
    height: height,
    adjustedHeight: height,
    inRecomputeQueue: false,
    nextInRecomputeQueue: none,
    name: "",

    // For initial `changedAt` we want a value lower than all possible stabilization numbers, but different than `stabilizationIsNotInProgress`.
    // Hence -2.
    changedAt: -2,
  };
}

// [[[cog
// immutable_fields = [
//   ['dependents', 'MutableArray SomeNode'],
//   ['observers', 'MutableArray (Observer a)'],
//   ['source', 'Source a'],
// ]
// mutable_fields = [
//   ['adjustedHeight', 'Int'],
//   ['changedAt', 'Int'],
//   ['height', 'Int'],
//   ['name', 'String'],
//   ['value', 'Optional a'],
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

export function get_source(node) {
  return node.source;
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
