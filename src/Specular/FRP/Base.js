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
