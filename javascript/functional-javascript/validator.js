function validator(message, fun) {
	var f = function(/* args */) {
    return fun.apply(fun, arguments);
  };

  f['message'] = message;
  return f;
}

function always(x) {
  return function() { return x; };
}

function wrap(fun) {
  return function(/* args */) { return fun.apply(fun, arguments); };
}

console.log(always(12)());
console.log(wrap(always)(12)());

var wrap2 = function(v) { return v; };

console.log(wrap2(always)(12)());


// Test 2
var _ = require('underscore');

function validator(message, fun) {
  var f = wrap(fun);
  f.message = message;
  return f;
}

function hasKeys(/* keys */) {
  var keys = _.toArray(arguments);

  return validator(["Must have values for keys:"].concat(keys).join(" "), function(obj) {
    return _.every(keys, function(k) { return _.has(obj, k); });
  });
}

console.log(hasKeys('msg', 'type').message);
console.log(hasKeys('msg', 'type')({ msg: "blah", type: "display" }));

var o = {
  msg: "blah"
};

var p = {
  __proto__: o,
  type: 'display',
};

console.log('msg' in p && 'type' in p);
console.log(hasKeys('msg', 'type')(p));
