var _ = require('underscore');

function visit(fun, collection) {
	if (typeof collection === 'object')
    _.each(collection, fun);
}

function deepVisit(fun, collection) {
	visit(function(c) { fun(c); deepVisit(fun, c); }, collection);
}

var x = [{ a: [1,2,3], b: 42 }, { c: { d: [] } }];

//deepVisit(function(o) { console.log(o, typeof o); }, x);

function freeze(x) { if (_.isObject(x)) Object.freeze(x); }

deepVisit(freeze, x);

deepVisit(function(o) { if (_.isObject(o)) console.log(o, Object.isFrozen(o)); }, x);

//console.log(x);


// Test _.isObject(x) vs. typeof x === 'object'

function tester(pred1, pred2) {
  return function(o) {
    return [o, pred1(o), pred2(o)];
  }
}

var objTester = tester(_.isObject, function(x) { return typeof x === 'object'; })

console.log([
  1,
  "",
  "a",
  [0,1],
  {a:1},
  null,
  undefined,
  function(){},
].map(objTester));
