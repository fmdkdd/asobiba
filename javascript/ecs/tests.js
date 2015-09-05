
var assert = require('assert')

/** Return a vector of coordinates (X,Y). */
function vector(x, y) { return {x, y}}
function vec_length(v) {
  return Math.sqrt(v.x*v.x + v.y*v.y)
}

assert(vec_length(vector(0,1)) === 0)
