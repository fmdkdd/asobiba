var counter = {
  c: 0,

  inc: function() { return ++this.c; },
  dec: function() { return --this.c; },
};

var resetCounter = {
  __proto__: counter,

  resetTimes: 0,
  reset: function() { this.c = 0; this.resetTimes++; },
};

counter.inc(); //: 1
counter.inc(); //: 2
counter.inc(); //: 3
resetCounter.c; //: 3

resetCounter.reset();
resetCounter.c; //: 0
counter.c; //: 3
resetCounter.inc(); //: 1
counter.c; //: 3

var Counter = {
  new: function() {
    return {__proto__: this, c: 0};
  },

  inc: function() { return ++this.c; },
  dec: function() { return --this.c; },
};

var ResetCounter = {
  __proto__: counter,

  new: function() {


    return {__proto__: this, resetTimes: 0};
  },

  reset: function() { this.c = 0; this.resetTimes++; },
};

var c = Counter.new();
var r = ResetCounter.new();

c.inc(); //: 1
c.inc(); //: 2
c.inc(); //: 3
r.c; //: 3

r.reset();
r.c; //: 0
c.c; //: 3
r.inc(); //: 1
c.c; //: 3

/* eslint no-underscore-dangle: 0 */

function o(def) {
  def._constructor = def.new || function() {};

  def.new = function() {
    var self = Object.create(def);  // {__proto__: this}
    def._constructor.apply(self, arguments);
    return self;
  };

  def.zuper = function() {
    Object.getPrototypeOf(def)._constructor.apply(this, arguments);
  };

  return def;
}

o.__proto__ === Function.prototype; //: true
o.__proto__.__proto__ === Object.prototype; //: true
o.__proto__.__proto__.__proto__ === null; //: true

typeof o.prototype; //: "object"
o.prototype.__proto__ === Object.prototype; //: true
o.prototype.__proto__.__proto__ === null; //: true

Counter.__proto__ === Object.prototype; //: true
Counter.__proto__.__proto__ === null; //: true

counter = o({
  new: function() {
    this.c = 0;
  },

  inc: function() { return ++this.c; },
  dec: function() { return --this.c; },

});

resetCounter = o({
  __proto__: counter,

  new: function() {
    this.zuper();
    this.resetTimes = 0;
  },

  reset: function() { this.c = 0; this.resetTimes++; },
});

c = counter.new();
r = resetCounter.new();

c.inc(); //: 1
c.inc(); //: 2
c.inc(); //: 3
c.c; //: 3

r.__proto__ === resetCounter; //: true
resetCounter.__proto__ === counter; //: true
r.c; //: 0
r.reset();
r.c; //: 0
c.c; //: 3
r.inc(); //: 1
c.c; //: 3

c = o({
  c: 0,
  inc: function() { return ++this.c; },
  dec: function() { return --this.c; },
});

c.inc(); //: 1
c.inc(); //: 2
c.inc(); //: 3
c.c; //: 3


function CCounter() {
  this.c = 0;
}

CCounter.prototype.inc = function() { return ++this.c; };
CCounter.prototype.dec = function() { return --this.c; };

c = new CCounter();
c.inc(); //: 1
c.inc(); //: 2
c.inc(); //: 3
c.c; //: 3


function RResetCounter() {
  CCounter.call(this);
  this.resetCount = 0;
}

RResetCounter.prototype = Object.create(CCounter.prototype);
RResetCounter.prototype.constructor = RResetCounter;
RResetCounter.prototype.reset = function() { this.resetCount++; this.c = 0; };

r = new RResetCounter();

r.c; //: 0
r.reset();
r.c; //: 0
c.c; //: 3
r.inc(); //: 1
c.c; //: 3
