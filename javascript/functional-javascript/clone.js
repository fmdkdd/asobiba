var _ = require('underscore');

function existy(x) {
	return x != null;
}

function deepClone(obj) {
	if (!existy(obj) || !_.isObject(obj))
    return obj;

  var temp = new obj.constructor();
  for (var key in obj)
    if (obj.hasOwnProperty(key))
      temp[key] = deepClone(obj[key]);

  return temp;
}

var x = { a: function(x) { return 2; } };

var y = deepClone(x);

console.log(x.a(), y.a());

x.a = 42;

console.log(x.a, y.a());



function betterDeepClone(obj) {
	if (!existy(obj) || typeof obj != 'object')
    return obj;

  var temp = new obj.constructor();
  for (var key in obj)
    if (obj.hasOwnProperty(key))
      temp[key] = betterDeepClone(obj[key]);

  return temp;
}

function makeCounter() {
  var x = 0;
  return function() { return ++x; };
}

var x = { a: function(x) { return 2; }, b: makeCounter() };

var y = betterDeepClone(x);

console.log(x.a(), x.b(), y.a(), y.b());

x.a = 42;

console.log(x.a, x.b(), y.a(), y.b());
