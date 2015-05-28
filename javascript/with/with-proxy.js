var o = {a: 1};

var h = {
	getPropertyDescriptor: function(name) {
		console.log('gpd', name);
		return Object.getOwnPropertyDescriptor(o, name);
	},

	counter: 0,

	get: function(receiver, name) {
		console.log(++this.counter, 'access to a');
		return o.a;
	},
};

var p = Proxy.create(h, o);

with(p) {
	console.log(a);
}
