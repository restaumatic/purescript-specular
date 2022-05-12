// pureImpl  :: forall r a.   a                                -> RIO r a
export function pureImpl(x) {
  return function RIO_pure_eff(_) {
    return x;
  };
}

// mapImpl   :: forall r a b. (a -> b)       -> RIO r a        -> RIO r b
export function mapImpl(f) {
  return function (io_x) {
    return function RIO_map_eff(env) {
      return f(io_x(env));
    };
  };
}

// applyImpl :: forall r a b. RIO r (a -> b) -> RIO r a        -> RIO r b
export function applyImpl(io_f) {
  return function (io_x) {
    return function RIO_apply_eff(env) {
      var f = io_f(env);
      return f(io_x(env));
    };
  };
}

// bindImpl  :: forall r a b. RIO r a        -> (a -> RIO r b) -> RIO r b
export function bindImpl(io_x) {
  return function (k) {
    return function RIO_bind_eff(env) {
      return k(io_x(env))(env);
    };
  };
}

// askImpl   :: forall r. RIO r r
export function askImpl(env) {
  return env;
}

// runRIO :: forall r a. r -> RIO r a -> IOSync a
export function runRIO(env) {
  return function (io) {
    return function runRIO_eff() {
      return io(env);
    };
  };
}

// rio :: forall r a. (r -> IOSync a) -> RIO r a
export function rio(f) {
  return function RIO_rio_eff(env) {
    return f(env)();
  };
}

// local :: forall r e a. (e -> r) -> RIO r a -> RIO e a
export function local(f) {
  return function (io) {
    return function RIO_local_eff(env) {
      return io(f(env));
    };
  };
}
