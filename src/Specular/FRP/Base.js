// Frame a ~ DelayedEffects -> Time -> IOSync Unit
// sequenceFrame_ :: Array (Frame Unit) -> Frame Unit
exports.sequenceFrame_ = function(xs) {
  return function(r1) {
    return function(r2) {
      return function sequenceFrame_eff() {
        for(var i = 0; i < xs.length; i++) {
          xs[i](r1)(r2)();
        }
      };
    };
  };
};
