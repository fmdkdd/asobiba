var _ = require('underscore');

function existy(x) {
	return x != null;
}

function cat(/* args */) {
	var head = _.first(arguments);
  if (existy(head))
    return head.concat.apply(head, _.rest(arguments));
  else
    return [];
}

function test(/* args */) {
  return cat([0], _.toArray(arguments));
  // _.union([0], arguments) works as well
}

console.log(test(1,2));
