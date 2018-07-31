// The PS widget is:
//
// void $ replicateM n $
//   elAttr "div" ("class" := "foo") $ do
//     elAttr "div" ("class" := "bar") $ do
//       text "foo"
//     elAttr "div" ("class" := "baz") $ do
//       text "foo"
//     elAttr "div" ("class" := "thud") $ do
//       text "foo"

// replicateM_Widget_ :: Int -> Widget Unit -> Widget Unit
// replicateM_, optimized for Widget.
exports.replicateM_Widget_ = function(n) {
  return function(widget) {
    return function(env) {
      for(var i = 0; i < n; i++) {
        widget(env);
      }
    };
  };
};

////////////////////////////////////////////////////////////////////////////////
// staticJS :: forall e. Int -> Eff e Unit
//
// This is what we're aiming for in terms of performance: imperative JS code
// that just creates and appends the nodes.
exports.staticJS = function(n) {
  return function() {
    var parent = document.createElement('div');
    for(var i = 0; i < n; i++) {
      elAttr('div', {'class':'foo'}, function(parent) {
        elAttr('div', {'class':'bar'}, function(parent) {
          text('foo', parent);
        }, parent);
        elAttr('div', {'class':'baz'}, function(parent) {
          text('foo', parent);
        }, parent);
        elAttr('div', {'class':'thud'}, function(parent) {
          text('foo', parent);
        }, parent);
      }, parent);
    }
  };
};

function elAttr(tag, attrs, content, parent) {
  var el = document.createElement(tag);
  for(var k in attrs) {
    el.setAttribute(k, attrs[k]);
  }
  content(el);
  parent.appendChild(el);
}

function text(t, parent) {
  var node = document.createTextNode(t);
  parent.appendChild(node);
}

////////////////////////////////////////////////////////////////////////////////
// staticJS_c :: forall e. Int -> Eff e Unit
//
// Like staticJS, but all functions are curried. This is more similar to what
// PureScript emits, but still uses imperative sequencing instead of `bindE`.

exports.staticJS_c = function(n) {
  return function() {
    var parent = document.createElement('div');
    for(var i = 0; i < n; i++) {
      elAttr_c('div')({'class':'foo'})(function(parent) {
        elAttr_c('div')({'class':'bar'})(function(parent) {
          text_c('foo')(parent);
        })(parent);
        elAttr_c('div')({'class':'baz'})(function(parent) {
          text_c('foo')(parent);
        })(parent);
        elAttr_c('div')({'class':'thud'})(function(parent) {
          text_c('foo')(parent);
        })(parent);
      })(parent);
    }
  };
};

function elAttr_c(tag) {
  return function(attrs) {
    return function(content) {
      return function(parent) {
        var el = document.createElement(tag);
        for(var k in attrs) {
          el.setAttribute(k, attrs[k]);
        }
        content(el);
        parent.appendChild(el);
      };
    };
  };
}

function text_c(t) {
  return function(parent) {
    var node = document.createTextNode(t);
    parent.appendChild(node);
  };
}

////////////////////////////////////////////////////////////////////////////////
// staticJS_m :: forall e. Int -> Eff e Unit
//
// Functions are curried, and sequencing is done using monadic `bind`, though
// specialized to `RIO` monad - "Reader + IO".

exports.staticJS_m = function(n) {
  return function() {
    var parent = document.createElement('div');
    replicateM_RIO(n,
      elAttr_c('div')({'class':'foo'})(
        bind_RIO( elAttr_c('div')({'class':'bar'})(text_c('foo')), function(_) {
          return bind_RIO( elAttr_c('div')({'class':'baz'})(text_c('foo')), function(_) {
            return elAttr_c('div')({'class':'thud'})(text_c('foo'));
          });
        })
      ))(parent);
  };
};

function bind_RIO(m, k) {
  return function(env) {
    return k(m(env))(env);
  };
};

// Generic implementation of `replicateM` in terms of `bind_RIO`.
function replicateM_RIO(n, x) {
  if(n == 0) {
    return function() {};
  } else {
    return bind_RIO(x, function(_) {
      return replicateM_RIO(n-1, x);
    });
  }
}
