var o = {a: 1};

var h = {
	getPropertyDescriptor: function(name) {
		print('gpd', name);
		return Object.getOwnPropertyDescriptor(o, name);
	},

	counter: 0,

	get: function(receiver, name) {
		print(++this.counter, 'access to a');
		return o.a;
	},
};

var p = Proxy.create(h, o);

with(p) {
	print(a);
}
