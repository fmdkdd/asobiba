var _ = require('underscore');

var reject = function(obj, iterator, context) {
	return _.filter(obj, function(value, index, list) {
		return !iterator(value, index, list);
	})
};

var even = function(x) {
	return x % 2 == 0;
};

console.log(reject([1,2,3,4], even));

console.log(_.reject([1,2,3,4], even));

// _.reject calls _.each directly
// reject calls _.filter, which calls _.each => slower
