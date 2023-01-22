import material_ from "material-components-web/dist/material-components-web.js";


// global.material = material_;

export const material = material_;

export function _new(cls, node) {
  return new cls(node);
}
