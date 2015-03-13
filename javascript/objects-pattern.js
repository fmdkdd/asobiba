var counter = {
  new(n) { return {__proto__: this, n} }
  ,get() { return this.n }
  ,inc() { this.n++; return this }}

// No need to redefine the `new` constructor, since `__proto__: this`
// will bind to decCounter.
var decCounter = {__proto__: counter
  ,dec() { this.n--; return this }}

var c = counter.new(0)
c.inc().get() //: 1

var d = decCounter.new(0)
d.inc().inc().dec().get() //: 1
