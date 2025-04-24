export function _new(none, source, dependents, observers, value, height) {
  const node = {
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
  traceEvent({
    type: "new",
    node: getId(node),
    dependencies: source.dependencies().map(getId),
  });
  return node;
}

class CycleError extends Error {
  nodes;

  constructor(node) {
    super("X11 Possible cycle detected");
    this.name = "CycleError";
    this.nodes = [node];
    console.error("Adding node to CycleError:", node.name);
  }

  toString() {
    return (
      super.toString() + ": " + this.nodes.map((node) => node.name).join(" -> ")
    );
  }
}

function findCycle(startNode) {
  function go(node) {
    if (node === startNode) {
      throw new Error("cycle found");
    }
    for (const dep of node.dependents.array) {
      console.log(`${node.name} <- ${dep.name}`);
      go(dep);
    }
  }
  for (const dep of startNode.dependents.array) {
    console.log(`${startNode.name} <- ${dep.name}`);
    go(dep);
  }
}

const ids = new Map();
let dumpCounter = 0;

function getId(n) {
  let id = ids.get(n);
  if (!id) {
    id = ids.size + 1;
    ids.set(n, id);
  }
  return id;
}

function qname(n) {
  return JSON.stringify(`${getId(n)}@${n.name.split(" ")[0]}`);
}

export const dumpGraph = (name, initialNode) => {
  const visited = new Set();

  let content = ["digraph {"];

  function go(n) {
    if (visited.has(n)) {
      return;
    }
    visited.add(n);

    for (const n2 of n.source.dependencies()) {
      if (!n2) {
        continue;
      }
      content.push(`${qname(n)} -> ${qname(n2)}`);
      go(n2);
    }
    for (const n2 of n.dependents.array) {
      if (!n2) {
        continue;
      }
      go(n2);
    }
  }

  getId(initialNode);

  for (const n of ids.keys()) {
    console.log(`dumping ${qname(n)}`);
    go(n);
  }

  content.push("}");

  global.debugFile(
    `specular/${leftPadZeros(dumpCounter)}-${shortenName(name)}.gv`,
    content.join("\n")
  );
  dumpCounter++;
};

function leftPadZeros(n) {
  return n.toString().padStart(4, "0");
}

function shortenName(name) {
  return name.substring(0, 100).replace(/ /g, "_").replace(/\.+$/, "");
}

export function _valueExc(Optional_none) {
  return (node) => {
    if (node.value === Optional_none) {
      dumpGraph("valueExc", node);
      throw new CycleError(node);
    }
    return node.value;
  };
}

export function catchCycleError(node, block) {
  try {
    return block();
  } catch (e) {
    if (e instanceof CycleError) {
      console.error("Adding node to CycleError:", node.name);
      e.nodes.push(node);
    }
    throw e;
  }
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
  traceAttrChange(node, "adjustedHeight", value);
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
  traceAttrChange(node, "height", value);
}

export function get_name(node) {
  return node.name;
}

export function set_name(node, value) {
  node.name = value;
  traceAttrChange(node, "name", value);
}

export function get_value(node) {
  return node.value;
}

export const set_value_impl = (Optional_none) => (node, value) => {
  const prevHasValue = node.value !== Optional_none;
  node.value = value;
  const hasValue = node.value !== Optional_none;
  if (prevHasValue !== hasValue) {
    traceAttrChange(node, "hasValue", hasValue);
  }
};

function traceAttrChange(node, attr, value) {
  traceEvent({
    type: "attrChange",
    node: getId(node),
    attr: attr,
    value: value,
  });
}

export function traceEdgeAdd(from, to) {
  traceEvent({
    type: "edgeAdd",
    from: getId(from),
    to: getId(to),
  });
}

export function traceEdgeRemove(from, to) {
  traceEvent({
    type: "edgeRemove",
    from: getId(from),
    to: getId(to),
  });
}

function traceEvent(event) {
  if (global.debugFile) {
    global.debugFile("specular/graph-log.txt", JSON.stringify(event) + "\n", {
      append: true,
    });
  }
}
// [[[end]]]
