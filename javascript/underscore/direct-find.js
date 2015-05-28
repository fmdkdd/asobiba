var _ = require('underscore');

// var find = function(obj, iterator, context) {
// 	var result;
//    _.each(obj, function(value, index, list) {
//       if (iterator.call(context, value, index, list)) {
// 			result = value;
// 			return breaker;
// 		}
//    });
//    return result;
// };

console.log(_.find([1,2,3,4], function(n) { return n > 2; }));
console.log(3 === _.find([1,2,3,4], function(n) { return n > 2; }));
console.log(undefined === _.find([1,2,3,4], function(n) { return false; }));
