import * as Specular_Internal_Incremental_Node from "../Specular.Internal.Incremental.Node/index.js";
import * as Specular_Internal_Incremental_MutableArray from "../Specular.Internal.Incremental.MutableArray/index.js";

export function addDependent(node, dependent) {
  var oldRefcount = Specular_Internal_Incremental_Node.refcount(node);
  node.dependents.push(dependent);
  handleRefcountChange(node, oldRefcount);
}
export function connect(node) {
  var dependencies = Specular_Internal_Incremental_Node.get_dependencies(node);
  for (const dependency of dependencies) {
    addDependent(dependency, node);
    var dependencyHeight = dependency.height;
    var ourHeight = node.height;
    var $35 = ((dependencyHeight + 1) | 0) > ourHeight;
    if ($35) {
      node.height = (dependencyHeight + 1) | 0;
      node.adjustedHeight = (dependencyHeight + 1) | 0;
    }
  }
  var value = Specular_Internal_Incremental_Node.compute(node);
  Specular_Internal_Incremental_Node.set_value(node, value);
}
export function disconnect(node) {
  for (const dependency of node.dependencies) {
    removeDependent(dependency, node);
  }
}
export function handleRefcountChange(node, oldRefcount) {
  var newcount = Specular_Internal_Incremental_Node.refcount(node);
  var $36 = oldRefcount === 0 && newcount > 0;
  if ($36) {
    connect(node);
  }
  var $37 = oldRefcount > 0 && newcount === 0;
  if ($37) {
    disconnect(node);
  }
  /*
  var oldTotalRefcount = Specular_Internal_Incremental_Ref.read(
    Specular_Internal_Incremental_Global.globalTotalRefcount
  );
  return Specular_Internal_Incremental_Ref.write(
    Specular_Internal_Incremental_Global.globalTotalRefcount,
    (((oldTotalRefcount - oldRefcount) | 0) + newcount) | 0
  );
  */
}
export function removeDependent(node, dependent) {
  var oldRefcount = Specular_Internal_Incremental_Node.refcount(node);
  var dependents = Specular_Internal_Incremental_Node.get_dependents(node);
  Specular_Internal_Incremental_MutableArray.remove(dependents, dependent);
  handleRefcountChange(node, oldRefcount);
}
