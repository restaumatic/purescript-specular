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

var nsid = 0;

// nextSwitchId :: IOSync Int
exports.nextSwitchId = function() {
  return nsid++;
};

var nsubid = 0;

exports.nextSubId = function() {
  return nsubid++;
};

var indentLevel = 0;

// debugN :: Int -> String -> IOSync Unit
exports.debugN = function(n) {
  return function(s) {
    return function() {
      if(n > 0) {
        indentLevel += n;
      }
      
      var indent = '';
      for(var i = 0; i < indentLevel; i++) {
        indent += ' ';
      }

      if(n < 0) {
        indentLevel += n;
      }

      console.log(indent + s);
    };
  };
};
