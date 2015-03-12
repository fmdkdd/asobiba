function test(a) {
  console.log(a, pc);
}

//console.log(test(12));

console.log('TEST 1');

try {
with ({pc: [1]}) {
  var a = 65;
  console.log(a, pc);
  console.log(test(42));
}
} catch(e) {
  console.log(e);
}

// Error: pc is not defined
// Because `with` cannot alter the lookup inside the `test` closure
// However

function test2(a, scope) {
  with(scope) {
    console.log(a, pc);
  }
}

console.log('TEST 2');

test2(65, {pc:[1]});

// This works, though we might not want to have to pass the scope as
// another argument.

// Instead we can do what is suggested here:
// http://www.bennadel.com/blog/2200-using-javascript-s-with-keyword-to-create-a-dynamic-scope-chain-for-method-execution.htm
// Use a dynamically-scoped function creator

var test3 = (function(){
  var scope = {};
  with(scope) {
    var fn = function(a) {
      console.log(a, pc);
    };
  }

  fn.scope = scope;

  return fn;
}());

console.log('TEST 3');

try {
  test3(65);
} catch (e) {
  console.log(e);
}

// ReferenceError: pc is not defined

console.log('TEST 4');

test3.scope.pc = [1];
test3(65); // works

// Note: test3.scope = {pc:[1]} fails since it erases the scope object
// used by `with`.

// Two problems remain with this solution:
// 1. We cannot create a `dynamicScope(fn, scope)` function that tells
// `fn` to search for bindings in `scope` first.
// 2. `with(scope)` is arbitrarily large.  What if we only want the
// `pc` binding to be dynamically-scoped?

function dynamicScope(fn) {
  var scope = {};
  with(scope) {
    eval("var fn = " + fn.toString());
  }

  fn.scope = scope;

  return fn;
}

console.log('TEST 5');

var test5 = dynamicScope(function(a) { console.log(a, pc); });
test5.scope.pc = [1];

test5(65);

// By using `eval` and fn.toString(), we effectively re-evaluate the
// function with a new lexical closure.  This works but has major
// restrictions:
// 1. fn.toString() is not defined for all functions.  Built-in
//    functions will fail miserably.
// 2. Erasing the closure of `fn` is incompatible with common patterns
//    like module.

// A variant that uses a fresh scope for each parameterized version of
// `test`.

function dynamicScope2(fn) {
  return function(scope) {
    with (scope) {
      eval("var dfn = " + fn.toString());
    }
    return dfn;
  };
}

function parameterize(dfn, env) {
  return dfn(env);
}

console.log('TEST 6');
var test6 = dynamicScope2(function(a) { console.log(a, pc); });
parameterize(test6, {pc: [1]})(65);

// Still vulnerable to the two previous points, but is compatible with
// an immutable style.
// Bonus points for using three controversial features in two lines:
// with, eval and function.toString.
