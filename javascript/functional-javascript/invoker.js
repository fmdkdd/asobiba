var _ = require('underscore');

function doWhen(cond, action) {
	if (truthy(cond))
    return action();
  else
    return undefined;
}

function truthy(x) {
	return (x !== false) && existy(x);
}

function existy(x) {
	return x != null;
}

var fail = console.error;

function invoker(name, method) {
	return function(target /* , args */) {
    if (!existy(target)) fail("Must provide a target");

    var targetMethod = target[name];
    var args = _.rest(arguments);

    return doWhen((existy(targetMethod) && method === targetMethod), function() {
      return targetMethod.apply(target, args);
    });
  };
}

// Much simpler

var caller = Function.prototype.bind.bind(Function.prototype.call);
// caller(f) ~ Function.prototype.call.bind(f)

var rev2 = caller([].reverse);
var rev3 = Function.prototype.call.bind([].reverse);

console.log(rev2([3,2,1]));
console.log(_.map([[3,2,1]], rev2));

console.log(rev3([3,2,1]));
console.log(_.map([[3,2,1]], rev3));

// Test

var rev = invoker('reverse', Array.prototype.reverse);

console.log(_.map([[3,2,1]], rev));

// Errors

//rev();
// "Must provide a target"
// followed by: Cannot read property 'reverse' of undefined

// rev2();
// Array.prototype.reverse called on null or undefined

// rev3();
// same as above
