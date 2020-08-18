exports._new = function(none, source, dependents, observers, value, height) {
  return {
    source: source,
    dependents: dependents,
    observers: observers,
    value: value,
    height: height,
    adjustedHeight: height,
    inRecomputeQueue: false,
    nextInRecomputeQueue: none,
    name: '',
    changedAt: 0,
  };
};

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
//     exports.get_%(name)s = function(node) { return node.%(name)s; };
//     """ % {'name': name})
// for name, _ in mutable_fields:
//     cog.outl("""
//     exports.get_%(name)s = function(node) { return node.%(name)s; };
//     exports.set_%(name)s = function(node, value) { node.%(name)s = value; };
//     """ % {'name': name})
// ]]]

exports.get_dependents = function(node) { return node.dependents; };


exports.get_observers = function(node) { return node.observers; };


exports.get_source = function(node) { return node.source; };


exports.get_adjustedHeight = function(node) { return node.adjustedHeight; };
exports.set_adjustedHeight = function(node, value) { node.adjustedHeight = value; };


exports.get_changedAt = function(node) { return node.changedAt; };
exports.set_changedAt = function(node, value) { node.changedAt = value; };


exports.get_height = function(node) { return node.height; };
exports.set_height = function(node, value) { node.height = value; };


exports.get_name = function(node) { return node.name; };
exports.set_name = function(node, value) { node.name = value; };


exports.get_value = function(node) { return node.value; };
exports.set_value = function(node, value) { node.value = value; };

// [[[end]]]
