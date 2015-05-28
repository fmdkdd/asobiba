var bindCall = Function.prototype.bind.call;

var context = { foo: "bar" };
function returnFoo() {
	return this.foo;
}

var bind = Function.prototype.call.bind(Function.prototype.bind);

var b = bind(returnFoo, context);

console.log(b()); // bar

var a = returnFoo.bind(context);

console.log(a()); // bar

// Hmmm

var trim = Function.prototype.call.bind(String.prototype.trim);
// trim(a,b,c) = Function.prototype.call(String.prototype.trim, a,b,c)
// f(a,b,c) = Function.prototype.call(f, a,b,c)
// f = Function.prototype.call.bind(f)
// so
// swap = Function.prototype.call.bind
// except swap() will have global as receiver, so
// swap = Function.prototype.call.bind.bind(Function.prototype.call.bind)

console.log(trim(" a ")); // 'a'

console.log(bind(String.prototype.trim, "a")()); // 'a'

// Version 1
var swap = function(f) {
  return function() {
    var args = [].slice.apply(arguments);
    return f.apply(args.shift(), args);
  }
};

trim = swap(String.prototype.trim);
var map = swap([].map);

console.log(trim(" a ")); // 'a'
console.log(map(["  a  ", '', '  '], trim)); // ['a', '', '']

// Version 2
swap = function(f) { return Function.prototype.call.bind(f) };

trim = swap(String.prototype.trim);
map = swap([].map);

console.log(trim(" a ")); // 'a'
console.log(map(["  a  ", '', '  '], trim)); // ['a', '', '']

// Version 3
swap = Function.prototype.call.bind.bind(Function.prototype.call.bind);

trim = swap(String.prototype.trim);
map = swap([].map);

// Wait ... I created thunks?!?!?
console.log(trim(" a ")()); // 'a'
console.log(map(["  a  ", '', '  '], trim)().map(function(d) { return d(); })); // ['a', '', '']

// Version 3b
swap = Function.prototype.call.bind.bind(Function.prototype.call);

trim = swap(String.prototype.trim);
map = swap([].map);

console.log(trim(" a ")); // 'a'
console.log(map(["  a  ", '', '  '], trim)); // ['a', '', '']

// Works because
// if g = x => a.b.c.f(x)
// we want to write
// g = a.b.c.f
// but need to bind
// g = a.b.c.f.bind(a.b.c)
// Had an extra `bind` (actually f) in version 3


// Version 4

var m2f = Function.prototype.bind.bind(Function.prototype.call);

var bind = m2f(m2f.bind);
m2f = bind(m2f.bind, m2f.call)

trim = m2f(String.prototype.trim);
console.log(trim(" a "));
