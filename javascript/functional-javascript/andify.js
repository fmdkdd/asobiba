function andify(/* preds */) {
  var preds = [].slice.apply(arguments);

	return function(/* args */) {
    var args = [].slice.apply(arguments);

    return args.every(function(a) { return preds.every(function(p) { return p(a); })});
  };
}

var evenNums = andify(function(n) { return typeof n == 'number'; },
                      function(n) { return n % 2 == 0; });

console.log(evenNums(1,2));
console.log(evenNums(2,4,6,8));
console.log(evenNums(2,4,6,8,9));


function orify(/* preds */) {
  var preds = [].slice.apply(arguments);

	return function(/* args */) {
    var args = [].slice.apply(arguments);

    return args.some(function(a) { return preds.some(function(p) { return p(a); })});
  };
}

var zeroOrOdd = orify(function(n) { return n % 2 == 1; },
                      function(n) { return n == 0; });

console.log(zeroOrOdd());
console.log(zeroOrOdd(0,2,4,6));
console.log(zeroOrOdd(2,4,6));

var _ = require('underscore');

function andify2(/* preds */) {
  var preds = arguments;

	return function(/* args */) {
    var args = arguments;

    return _.all(args, function(a) { return _.all(preds, function(p) { return p(a); })});
  };
}

var evenNums = andify2(function(n) { return typeof n == 'number'; },
                       function(n) { return n % 2 == 0; });

console.log(evenNums(1,2));
console.log(evenNums(2,4,6,8));
console.log(evenNums(2,4,6,8,9));
