// sequenceFrame_ :: Array (Frame Unit) -> Frame Unit
exports.sequenceFrame_ = function(xs) {
  return function sequenceFrame_eff(env) {
    for(var i = 0; i < xs.length; i++) {
      xs[i](env);
    }
  };
};
