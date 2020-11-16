A FRP computation is represented using a directed graph of dependencies.

For example, `map f d` (assuming `d :: Dynamic T`) is represented as `Node` which has a dependency edge to `d`.
Edge pointers are maintained in both directions.

There are two kinds of vertices in this graph:
- `Node a`, representing a computation computing a value of type `a`,
- `Observer a`, an event handler which gets notified if a `Node`'s value changes.

Some nodes are _roots_, i.e. they don't have any dependencies. Their value is changed from outside the system.
For example `newDynamic` creates a new root.

# Node

`Node` is a record type with mutable fields. Because PureScript doesn't support that directly, it is implemented using FFI.

We use a hack to be able to store in a single collection nodes of different types:

```purescript
foreign import data Any :: Type
type SomeNode = Node Any
```

`Node` has the following properties:

- `dependents :: MutableArray SomeNode` - obviously `Nodes` dependent on this one
- `observers :: MutableArray (Observer a)` - Observers which observe this node
- `source :: Source a` - the "definition" of the node which is specific to the node type. See below.
- mutable `height :: Int` - Maximum distance to any root. Used to traverse nodes in proper dependency order.
- mutable `adjustedHeight :: Int` - Heights are updated lazily. This is the next height value.
- mutable `changedAt :: Int` - Logical timestamp of when this node changed. Used for `Events` to determine if a node is currently changing.
- mutable `name :: String` - Optional name given for debugging purposes.
- mutable `value :: Optional a` - The current value, or `none` if not computed yet.

`Source` is the following:

- `compute :: EffectFn1 (Node a) (Optional a)` - Compute the node value, if none is returned then we don't change value and don't propagate.
- `dependencies :: Effect (Array SomeNode)` - nodes which depend on this one. This is an `Effect` because they may change, and are computed dynamically in some cases (notably `bind`).

# Connectivity

Each node has a _reference count_, which is computed as the number of dependent nodes plus number of observers.
If the reference count is zero, the node is said to be _disconnected_; otherwise it is _connected_.

Node height is maintained only when the node is connected. Therefore it must be recomputed when a node becomes connected.

# Utility types

## Optional

Optional is an unboxed variant of Maybe. It doesn't need to allocate memory to wrap a value in `Just` (`some` in Optional terminology).
The disadantage is that it can't be nested - `some none` has the same representation as `none`, so it should be used with great care.
Notably, if you use `Dynamic (Optional a)`, things will break.
