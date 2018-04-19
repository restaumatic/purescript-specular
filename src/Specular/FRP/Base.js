// sequenceFrame_ :: Array (Frame Unit) -> Frame Unit
exports.sequenceFrame_ = function(xs) {
  return function sequenceFrame_eff(env) {
    for(var i = 0; i < xs.length; i++) {
      xs[i](env);
    }
  };
};

// oncePerFrame_ :: Frame Unit -> IOSync (Frame Unit)
// oncePerFrame_ action = do
//   ref <- newIORef Nothing
//   pure $ do
//     time <- framePull $ getTime
//     m_lastTime <- framePull $ pullReadIORef ref
//     case m_lastTime of
//       Just lastTime | lastTime == time ->
//         pure unit
//       _ -> do
//         frameWriteIORef ref (Just time)
//         action
exports.oncePerFrame_ = function(action) {
  return function() {
    var lastTime = null;
    return function oncePerFrame_eff(env) {
      if(lastTime !== env.time) {
        lastTime = env.time;
        action(env);
      }
    };
  };
};


// oncePerFramePullWithIO :: forall a b. Pull a -> (a -> IOSync b) -> IOSync (Pull b)
// oncePerFramePullWithIO action io = do
//   ref <- newIORef Fresh
//   pure $ unsafeMkPull $ \time -> do
//     cache <- readIORef ref
//     case cache of
//       Cached lastTime value | lastTime == time ->
//         pure value
// 
//       BlackHole ->
//         unsafeCrashWith "Illegal self-referential computation passed to oncePerFrame"
// 
//       _ -> do
//         writeIORef ref BlackHole
//         value <- runPull time action >>= io
//         writeIORef ref (Cached time value)
//         pure value
var FRESH = 0;
var BLACK_HOLE = 1;
var CACHED = 2;
exports.oncePerFramePullWithIO = function(action) {
  return function(io) {
    return function() {
      var state = FRESH;
      var cachedTime;
      var cachedValue;

      return function oncePerFramePullWithIO_eff(env) {
        var currentTime = env;

        if (state == CACHED && cachedTime == currentTime) {
          return cachedValue;
        } else if(state == BLACK_HOLE) {
          throw new Error("Illegal self-referential computation passed to oncePerFrame");
        } else {
          state = BLACK_HOLE;
          cachedValue = io(action(env))();
          cachedTime = currentTime;
          state = CACHED;
          return cachedValue;
        }
      };
    };
  };
};
