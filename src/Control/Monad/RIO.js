// pureImpl  :: forall r a.   a                                -> RIO r a
exports.pureImpl = function(x) {
  return function(_) {
    return x;
  };
};

// mapImpl   :: forall r a b. (a -> b)       -> RIO r a        -> RIO r b
exports.mapImpl = function(f) {
  return function(io_x) {
    return function(env) {
      return f(io_x(env));
    };
  };
};

// applyImpl :: forall r a b. RIO r (a -> b) -> RIO r a        -> RIO r b
exports.applyImpl = function(io_f) {
  return function(io_x) {
    return function(env) {
      var f = io_f(env);
      return f(io_x(env));
    };
  };
};

// bindImpl  :: forall r a b. RIO r a        -> (a -> RIO r b) -> RIO r b
exports.bindImpl = function(io_x) {
  return function(k) {
    return function(env) {
      return k(io_x(env))(env);
    };
  };
};

// askImpl   :: forall r. RIO r r
exports.askImpl = function(env) {
  return env;
};

// runRIO :: forall r a. r -> RIO r a -> IOSync a
exports.runRIO = function(env) {
  return function(io) {
    return function() {
      return io(env);
    };
  };
};

// rio :: forall r a. (r -> IOSync a) -> RIO r a
exports.rio = function(f) {
  return function(env) {
    return f(env)();
  };
};

// local :: forall r e a. (e -> r) -> RIO r a -> RIO e a
exports.local = function(f) {
  return function(io) {
    return function(env) {
      return io(f(env));
    };
  };
};
