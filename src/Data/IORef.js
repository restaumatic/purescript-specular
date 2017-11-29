// module Data.IORef
// data IORef :: Type -> Type

// newIORef :: forall a. a -> IOSync (IORef a)
exports.newIORef = function(initial) {
  return function() {
    return {
      value: initial
    };
  };
};

// readIORef :: forall a. IORef a -> IOSync a
exports.readIORef = function(ref) {
  return function() {
    return ref.value;
  };
};

// writeIORef :: forall a. IORef a -> a -> IOSync Unit
exports.writeIORef = function(ref) {
  return function(newValue) {
    return function() {
      ref.value = newValue;
    };
  };
};
