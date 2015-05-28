var _ = require('underscore');

_.each([1,2,3,4], console.log);

// Works with float length
_.each({length: 1.2, 0: 0, 1: 1, 2: 2}, console.log);

// Thus with Infinity!
_.each({length: Infinity}, function(_, i) {
	if (i > 10) break;
});
