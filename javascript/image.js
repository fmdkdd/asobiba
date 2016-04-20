/* eslint-disable */

var Image = {
  width: 0,
  height: 0,
  pixels: [],

  new(w, h) {
    var o = Object.create(this)

    o.width = w
    o.height = h
    o.pixels = new Array(w * h)
    o.pixels.fill(0)

    return o
  },

  pixel(x, y, v) {
    if (v) {
      this.pixels[y * this.width + x] = v
    }
    else {
      return this.pixels[y * this.width + x]
    }
  },
}

function or(p, q) { return p | q }
function and(p, q) { return p & q }
function not(p) { return 1^p }

function Or(a, b) {
  var result = Image.new(a.width, a.height)

  for (var x=0; x < a.width; ++x) {
    for (var y=0; y < a.height; ++y) {
      result.pixel(x, y,
                   or(a.pixel(x, y), b.pixel(x, y)))
    }
  }

  return result
}


function And(a, b) {
  var result = Image.new(a.width, a.height)

  for (var x=0; x < a.width; ++x) {
    for (var y=0; y < a.height; ++y) {
      result.pixel(x, y,
                   and(a.pixel(x, y), b.pixel(x, y)))
    }
  }

  return result
}


function Not(a) {
  var result = Image.new(a.width, a.height)

  for (var x=0; x < a.width; ++x) {
    for (var y=0; y < a.height; ++y) {
      result.pixel(x, y, not(a.pixel(x, y)))
    }
  }

  return result
}

function Down(a) {
  var result = Image.new(a.width, a.height)

  for (var y=0; y < a.height - 1; ++y) {
    for (var x=0; x < a.width; ++x) {
      result.pixel(x, y + 1, a.pixel(x, y))
    }
  }

  return result
}

function Remove(a, b) {
  return And(a, Not(b))
}

function TopEdge(a) {
  return Remove(a, Down(a))
}

var a = Image.new(5, 2)
a.pixels[2] = 1
a.pixels[3] = 1
a.pixels[4] = 1
var b = Image.new(5, 2)
b.pixels.fill(1)

a //: Object {width:5,height:2,pixels:[0,0,1,1,1,0,0,0,0,0]}
b //: Object {width:5,height:2,pixels:[1,1,1,1,1,1,1,1,1,1]}

Or(a, b) //: Object {width:5,height:2,pixels:[1,1,1,1,1,1,1,1,1,1]}
Down(b) //: Object {width:5,height:2,pixels:[0,0,0,0,0,1,1,1,1,1]}

Remove(b, a) //: Object {width:5,height:2,pixels:[1,1,0,0,0,1,1,1,1,1]}
TopEdge(a) //: Object {width:5,height:2,pixels:[0,0,1,1,1,0,0,0,0,0]}
TopEdge(b) //: Object {width:5,height:2,pixels:[1,1,1,1,1,0,0,0,0,0]}
