// sequenceFrame_ :: Array (Frame Unit) -> Frame Unit
exports.sequenceFrame_ = function(xs) {
  return function sequenceFrame_eff(env) {
    for(var i = 0; i < xs.length; i++) {
      xs[i](env);
    }
  };
};

// unsafeMkPull :: forall a. (Time -> IOSync a) -> Pull a
exports.unsafeMkPull = function(f) {
  return function(env) {
    return f(env)();
  };
};
