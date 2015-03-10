var lengths = {
  object: function(obj) {
    var props = 0
    var propLen = 0


    for (var key in obj) {
      if (obj.hasOwnProperty(key)) {
        ++props
        propLen += length(obj[key]) }}

    return props + propLen + 1 },

  number: function(n) {
    return 4 }, }

var packs = {
  keyMap: {
    'angle': 0,
    'x': 1,
    'y': 2
  },

  object: function(obj, view, offset) {
    var props = Object.getOwnPropertyNames(obj)
    view.setUint8(offset, props.length)
    offset += 1

    props.forEach(function(key) {
      view.setUint8(offset, packs.keyMap[key])
      offset += 1
      offset = packValue(obj[key], view, offset) })

    return offset
  },

  number: function(n, view, offset) {
    view.setFloat32(offset, n)
    return offset + 4
  }, }

function dispatch(f, o, view, offset) {
  if (typeof o === 'number')
    return f.number(o, view, offset)
  else if (typeof o === 'object')
    return f.object(o, view, offset)
  else
    throw new Error('Cannot dispatch on value type ' + typeof o) }

function pack(o) {
  var buffer = new ArrayBuffer(length(o))
  var view = new DataView(buffer)
  packValue(o, view, 0)
  return view }

var length = dispatch.bind(null, lengths)
var packValue = dispatch.bind(null, packs)

var testNum = 2
var testObj = {
  'angle': Math.random(),
  'x': Math.random(),
  'y': Math.random(),
}

console.log(pack(testNum).getFloat32(0))
console.log(pack(testObj).byteLength) // 16 bytes

var msgpack = require('msgpack5')()
console.log(msgpack.encode(testObj).length) // 26 bytes

/* Preliminary conclusion
 *
 * A custom packing would certainly lower packet size, even if only
 * for the most common messages: objects update and keys.
 *
 * However, there is a significant impact on performance.  This simple
 * custom pack is already 2x slower than a JSON.stringify.  Every
 * milliseconds counts for getting 60fps on the client, and 50fps on
 * the server.  Since the game is playable in a LAN environment,
 * message size may be less important than speed of
 * serialization/deserialization.
 *
 * Besides, we are already losing ~60 bytes to the TCP wrapper, so it
 * may be more important to bundle messages rather than sending a
 * bunch of small messages.
 */

//
However,
