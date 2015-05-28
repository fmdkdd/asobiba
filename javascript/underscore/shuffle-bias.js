var _ = require('underscore');

var array = [0,1,2,3,4];

var dist = [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]];

for (var i = 0; i < 10000000; ++i) {
	var shuffled = _.shuffle(array);
	for (var j = 0; j < dist.length; ++j)
		dist[j][shuffled[j]]++;
}

console.log(dist);


// [ 1, 3, 2, 0 ]
// [ 1, 2, 3, 0 ]
// [ 2, 3, 1, 0 ]

// dist[0][1]++
// dist[1][3]++
// dist[2][2]++
// dist[3][0]++
