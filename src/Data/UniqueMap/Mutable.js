// data UniqueMap a = mutable Object
// data Unique = Number

// A global counter
var nextUnique = 0;

// new :: forall a. IOSync (UniqueMap a)
exports.new = function() {
  return {};
};

// insert :: forall a. a -> UniqueMap a -> IOSync Unique
exports.insert = function(value) {
  return function(map) {
    return function() {
      var key = nextUnique++;
      map[key] = value;
      return key;
    };
  };
};

// lookupImpl :: forall a. Unique -> UniqueMap a -> (a -> Maybe a) -> Maybe a -> IOSync (Maybe a)
exports.lookupImpl = function(key) {
  return function(map) {
    return function(Just) {
      return function(Nothing) {
        return function() {
          if(key in map) {
            return Just(map[key]);
          } else {
            return Nothing;
          }
        };
      };
    };
  };
};
        
// delete :: forall a. Unique -> UniqueMap a -> IOSync Unit
exports['delete'] = function(key) {
  return function(map) {
    return function() {
      delete map[key];
    };
  };
};

// values :: forall a. UniqueMap a -> IOSync (Array a)
exports.values = function(map) {
  return function() {
    return Object.keys(map).map(function(key) {
      return map[key];
    });
  };
};
