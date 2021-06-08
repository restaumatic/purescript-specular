// _stopPropagation :: EffectFn1 DOM.Event Unit
exports._stopPropagation = function(event) {
  event.stopPropagation();
};

// _addClass :: EffectFn2 Node ClassName Unit
exports._addClass = function(node, cls) {
  node.classList.add(...cls.split(' '));
};

// _initClasses :: EffectFn1 Node (EffectFn1 (Array ClassName) Unit)
exports._initClasses = function(node) {
  var currentClassSet = {};
  return function(classes) {
    var newClassSet = {};
    for(var i = 0; i < classes.length; i++) {
      for(const class_ of classes[i].split(' ')) {
        newClassSet[class_] = true;
        if(!currentClassSet[class_]) {
          node.classList.add(class_);
        }
      }
    }
    var oldClasses = Object.keys(currentClassSet);
    for(var i = 0; i < oldClasses.length; i++) {
      var class_ = oldClasses[i];
      if(!newClassSet[class_]) {
        node.classList.remove(class_);
      }
    }
    currentClassSet = newClassSet;
  }
};
