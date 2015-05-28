var _ = require('underscore');

function partial(fun /* , pargs */) {
	var pargs = _.rest(arguments);

  return function(/* arguments */) {
	  var args = _.union(pargs, arguments);
    return fun.apply(fun, args)
  }
}

function div(n, d) {
	return n / d;
}

var partialDiv = partial(div, 10, 2, 4, 5000);
console.log(partialDiv());

function checkedPartial(fun /* , pargs */) {
	var pargs = _.rest(arguments);

  if (pargs.length > fun.length) throw "Too many arguments";

  return function(/* arguments */) {
	  var args = _.union(pargs, arguments);
    return fun.apply(fun, args)
  }
}

var partialDiv2 = checkedPartial(div, 10, 5);
// Fails
console.log(partialDiv2());
