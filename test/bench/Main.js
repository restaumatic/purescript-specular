exports.exportBenchmark = function() {
  window.Benchmark = require('benchmark');
};

// staticJS :: forall e. Int -> Eff e Unit
// void $ replicateM n $
//   elAttr "div" ("class" := "foo") $ do
//     elAttr "div" ("class" := "bar") $ do
//       text "foo"
//     elAttr "div" ("class" := "baz") $ do
//       text "foo"
//     elAttr "div" ("class" := "thud") $ do
//       text "foo"
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
//    console.log('html', parent.outerHTML);
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

exports.staticJS_m = function(n) {
  return function() {
    var parent = document.createElement('div');
    replicateM_RIO(n,
      elAttr_c('div')({'class':'foo'})(function(parent) {
        elAttr_c('div')({'class':'bar'})(text_c('foo'))(parent);
        elAttr_c('div')({'class':'baz'})(text_c('foo'))(parent);
        elAttr_c('div')({'class':'thud'})(text_c('foo'))(parent);
      }))(parent);
//    console.log('html', parent.outerHTML);
  };
};

function bind_RIO(m, k) {
  return function(env) {
    return k(m(env))(env);
  };
};

function replicateM_RIO(n, x) {
  if(n == 0) {
    return function() {};
  } else {
    return bind_RIO(x, function(_) {
      return replicateM_RIO(n-1, x);
    });
  }
}
