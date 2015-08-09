// Trying out the ECS pattern as described here:
// http://www.gamedev.net/page/resources/_/technical/game-programming/implementing-component-entity-systems-r3382

// TODO: time-based updates rather than discrete increments.  Gameplay becomes
// independent of frame rate.

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Engine

// FIXME: this is getting out of hand
var C_NONE                = 0b0,
    C_POSITION            = 0b1,
    C_VELOCITY            = 0b10,
    C_RENDERABLE          = 0b100,
    C_INPUT               = 0b1000,
    C_ROTATION            = 0b10000,
    C_TANK_CONTROL        = 0b100000,
    C_INPUT_KEYBOARD      = 0b1000000,
    C_INPUT_GAMEPAD       = 0b10000000,
    C_ROTATOR             = 0b100000000,
    C_BULLET_CANNON       = 0b1000000000,
    C_DESTROY_OUT_OF_VIEW = 0b10000000000,
    C_DIRECT_CONTROL      = 0b100000000000,
    C_BOUNDING_BOX        = 0b1000000000000,
    C_ASTEROID            = 0b10000000000000,
    C_BULLET              = 0b100000000000000

// Make room for a few items.  FIXME: the array is not actually filled with data
// until a create* function is called.
var entities_count = 100

var world = {
  mask: new Array(entities_count),
  position: new Array(entities_count),
  velocity: new Array(entities_count),
  renderable: new Array(entities_count),
  input: new Array(entities_count),
  rotation: new Array(entities_count),
  rotatorSpeed: new Array(entities_count),
  size: new Array(entities_count),
  shape: new Array(entities_count),
  boundingBox: new Array(entities_count),
  hits: new Array(entities_count),
}

var spatialHashCellSize = 100
var grid = spatialHash.new(spatialHashCellSize)

function createEntity(world) {
  var i = 0
  while (world.mask[i] !== C_NONE && world.mask[i] != null)
    ++i

  return i
}

function destroyEntity(world, e) {
  world.mask[e] = C_NONE
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Moving

var moveMask = C_POSITION | C_VELOCITY

function move(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & moveMask) === moveMask) {
      var p = world.position[e]
      var v = world.velocity[e]
      p.x += v.x
      p.y += v.y

      if ((world.mask[e] & C_BOUNDING_BOX) === C_BOUNDING_BOX) {
        var b = world.boundingBox[e]
        b.x += v.x
        b.y += v.y
      }
    }
  }
}

var tankControlMask = C_TANK_CONTROL | C_INPUT | C_ROTATION | C_VELOCITY

var acceleration = 0.175
var angularSpeed = 0.1

function tankControl(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & tankControlMask) === tankControlMask) {
      var input = world.input[e]

      if (input[I_ROTATE_LEFT])
        world.rotation[e] -= angularSpeed
      if (input[I_ROTATE_RIGHT])
        world.rotation[e] += angularSpeed
      if (input[I_THRUST]) {
        var v = world.velocity[e]
        var r = world.rotation[e]
        v.x += acceleration * Math.cos(r)
        v.y += acceleration * Math.sin(r)
      }
    }
  }
}

var directControlMask = C_DIRECT_CONTROL | C_INPUT | C_ROTATION | C_VELOCITY

var directControlSpeed = 4

function directControl(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & directControlMask) === directControlMask) {
      var input = world.input[e]

      if (input[I_ROTATE_LEFT])
        world.rotation[e] -= angularSpeed
      if (input[I_ROTATE_RIGHT])
        world.rotation[e] += angularSpeed
      if (input[I_THRUST]) {
        var v = world.velocity[e]
        var r = world.rotation[e]
        v.x = directControlSpeed * Math.cos(r)
        v.y = directControlSpeed * Math.sin(r)
      }
    }
  }
}

var rotatorMask = C_ROTATION | C_ROTATOR

function rotator(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_ROTATOR) === C_ROTATOR) {
      world.rotation[e] += world.rotatorSpeed[e]
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Gameplay

var bulletCannonMask = C_BULLET_CANNON | C_INPUT
var bulletInitialSpeed = 5
var cannonHeat = 0
var maxCannonHeat = 10

function bulletCannon(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & bulletCannonMask) === bulletCannonMask) {
      var input = world.input[e]

      cannonHeat = Math.max(0, cannonHeat - 1)

      if (cannonHeat === 0 && input[I_FIRE]) {
        var p = world.position[e]
        var r = world.rotation[e]

        var bulletPosition = {
          x: p.x + (10 * Math.cos(r)),
          y: p.y + (10 * Math.sin(r))
        }

        var v = world.velocity[e]
        var bulletVelocity = {
          x: v.x + bulletInitialSpeed * Math.cos(r),
          y: v.y + bulletInitialSpeed * Math.sin(r)
        }

        createBullet(world, bulletPosition, bulletVelocity)
        cannonHeat = maxCannonHeat
      }
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Engine

var strayEntitiesCollectorMask = C_DESTROY_OUT_OF_VIEW | C_BOUNDING_BOX

function strayEntitiesCollector(world) {
  var screen = {x:0, y:0, width: ctx.canvas.width, height: ctx.canvas.height}

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & strayEntitiesCollectorMask)
        === strayEntitiesCollectorMask) {
      var b = world.boundingBox[e]

      if (!do_boxes_collide(b, screen))
        destroyEntity(world, e)
    }
  }
}

var collisionDetectionMask = C_BOUNDING_BOX

function collisionDetection(world) {
  grid.clearAllCells()

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & collisionDetectionMask)
        === collisionDetectionMask) {
      var b = world.boundingBox[e]
      grid.insertObjectWithBoundingBox(e, b)
      world.hits[e].clear()
    }
  }

  for (var objSet of grid.map.values()) {
    var objs = Array.from(objSet)
    for (var i = 0; i < objs.length; ++i) {
      var e1 = objs[i]
      var b1 = world.boundingBox[e1]
      for (var j = i+1; j < objs.length; ++j) {
        var e2 = objs[j]
        var b2 = world.boundingBox[e2]
        if (do_boxes_collide(b1, b2)) {
          world.hits[e1].add(e2)
          world.hits[e2].add(e1)
        }
      }
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Input

var keys = Object.create(null)

// Keyboard keycodes
var K_LEFT  = 37,
    K_UP    = 38,
    K_RIGHT = 39,
    K_SPACE = 32

// Game controls
var I_ROTATE_LEFT  = 0,
    I_ROTATE_RIGHT = 1,
    I_THRUST       = 2,
    I_FIRE         = 3


function clearInput(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_INPUT) === C_INPUT) {
      var input = world.input[e]

      // Clear input
      for (var k in input) input[k] = false
    }
  }
}

function keyboardControl(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_INPUT_KEYBOARD) === C_INPUT_KEYBOARD) {
      var input = world.input[e]

      if (keys[K_LEFT]) input[I_ROTATE_LEFT] = true
      if (keys[K_RIGHT]) input[I_ROTATE_RIGHT] = true
      if (keys[K_UP]) input[I_THRUST] = true
      if (keys[K_SPACE]) input[I_FIRE] = true
    }
  }
}

function initKeyListeners() {
  window.addEventListener('keydown', onKeyDown)
  window.addEventListener('keyup', onKeyUp)
}

function onKeyDown(ev) { keys[ev.which] = true }
function onKeyUp(ev) { keys[ev.which] = false }

document.addEventListener('DOMContentLoaded', initKeyListeners)


var AXIS_THRESHOLD = 0.5

function gamepadControl(world) {
  var pads = navigator.getGamepads()
  var pad = pads[0]

  if (!pad) return

  var axes = pad.axes
  var buttons = pad.buttons

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_INPUT_GAMEPAD) === C_INPUT_GAMEPAD) {
      var input = world.input[e]

      if (axes[0] < -AXIS_THRESHOLD) input[I_ROTATE_LEFT] = true
      if (axes[0] > AXIS_THRESHOLD) input[I_ROTATE_RIGHT] = true
      if (buttons[6].pressed) input[I_THRUST] = true
      if (buttons[0].pressed) input[I_FIRE] = true
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Rendering

var fixedCamera = {
  new(x, y) {
    return {
      __proto__: this,
      x, y
    }
  },

  vec() { return {x: this.x, y: this.y} },
}.new(0,0)

var focusedCamera = {
  new(id) {
    return {
      __proto__: this,
      id
    }
  },

  vec() {
    var p = world.position[this.id]
    var cx = ctx.canvas.width / 2
    var cy = ctx.canvas.height / 2
    return { x: cx - p.x, y: cy - p.y }
  },
}

var ctx
var camera = fixedCamera

function render(world, ctx) {
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  ctx.save()
  ctx.translate(camera.vec().x, camera.vec().y)

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_RENDERABLE) === C_RENDERABLE) {
      world.renderable[e](e, ctx)
    }
  }

  ctx.restore()
}

function initCanvas() {
  var canvas = document.getElementById('canvas')
  canvas.width = window.innerWidth - 5
  canvas.height = window.innerHeight - 5
  ctx = canvas.getContext('2d')

  world.width = canvas.width
  world.height = canvas.height
}

document.addEventListener('DOMContentLoaded', initCanvas)

function renderShip(e, ctx) {
  var x = world.position[e].x
  var y = world.position[e].y
  var angle = world.rotation[e]

  ctx.save()

  ctx.strokeStyle = 'hsl(180,70%,30%)'
  ctx.lineJoin = 'round'
  ctx.lineCap = 'round'
  ctx.translate(x, y)
  ctx.scale(2,2)
  ctx.rotate(angle)

  ctx.beginPath()
  ctx.moveTo(5, 0)
  ctx.lineTo(-5, -4)
  ctx.lineTo(-5, 4)
  ctx.closePath()
  ctx.stroke()

  ctx.restore()
}

function renderAsteroid(e, ctx) {
  var x = world.position[e].x
  var y = world.position[e].y
  var angle = world.rotation[e]
  var size = world.size[e]
  var shape = world.shape[e]

  ctx.save()

  ctx.lineWidth = 0.05
  ctx.strokeStyle = 'hsl(0,70%,10%)'
  ctx.lineJoin = 'round'
  ctx.lineCap = 'round'
  ctx.translate(x, y)
  ctx.scale(size, size)
  ctx.rotate(angle)

  ctx.beginPath()
  ctx.moveTo(shape[0].x, shape[0].y)
  for (var i = 1; i < shape.length; ++i) {
    ctx.lineTo(shape[i].x, shape[i].y)
  }
  ctx.closePath()
  ctx.stroke()

  ctx.restore()
}

function renderBullet(e, ctx) {
  var x = world.position[e].x
  var y = world.position[e].y

  ctx.save()

  ctx.strokeStyle = 'hsl(180,70%,30%)'
  ctx.translate(x, y)
  ctx.scale(3,3)

  ctx.beginPath()
  ctx.arc(0, 0, 1, 0, Math.PI * 2)
  ctx.stroke()

  ctx.restore()
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Entities

function createShip(world, x, y) {
  var e = createEntity(world)

  world.mask[e] = C_POSITION
    | C_VELOCITY
    | C_ROTATION
    | C_RENDERABLE
    | C_TANK_CONTROL
    | C_INPUT
    | C_BULLET_CANNON
    | C_BOUNDING_BOX

  world.position[e] = {x, y}
  world.velocity[e] = {x: 0, y: 0}
  world.rotation[e] = 0
  world.input[e] = Object.create(null)

  world.renderable[e] = renderShip

  var size = 25
  world.boundingBox[e] = {x: x - size/2, y: y - size/2,
                          width: size, height: size}
  world.hits[e] = new Set()

  return e
}

function createAsteroid(world, x, y, size, velx, vely) {
  var e = createEntity(world)

  world.mask[e] = C_POSITION
    | C_VELOCITY
    | C_ROTATION
    | C_ROTATOR
    | C_RENDERABLE
    | C_BOUNDING_BOX
    | C_DESTROY_OUT_OF_VIEW
    | C_ASTEROID

  world.position[e] = {x, y}
  world.velocity[e] = {x: velx || 0, y: vely || 0}
  world.rotation[e] = 0

  if (size == null)
    size = 40 + Math.random() * 100
  world.size[e] = size
  world.rotatorSpeed[e] = 1 / size

  world.shape[e] = []
  for (var i = 0, a = 0; i < 6; ++i, a += Math.PI /3) {
    var m = 0.4 + Math.random()/3
    world.shape[e].push({x: m* Math.cos(a), y: m * Math.sin(a)})
  }

  world.renderable[e] = renderAsteroid

  world.boundingBox[e] = {x: x - size/2, y: y - size/2,
                          width: size, height: size}
  world.hits[e] = new Set()

  return e
}

var minAsteroidSize = 30
var asteroidSpeedFactor = 100

function fragmentAsteroid(e) {
  var p = world.position[e]
  var s = world.size[e]

  destroyEntity(world, e)

  if (s > minAsteroidSize) {
    var r = {x: Math.random() * asteroidSpeedFactor / s,
             y: Math.random() * asteroidSpeedFactor / s}
    createAsteroid(world, p.x, p.y, s/2, r.x, r.y)
    createAsteroid(world, p.x, p.y, s/2, r.y, -r.x)
  }
}

function createBullet(world, position, velocity) {
  var e = createEntity(world)

  world.mask[e] = C_POSITION
    | C_VELOCITY
    | C_RENDERABLE
    | C_DESTROY_OUT_OF_VIEW
    | C_BOUNDING_BOX
    | C_BULLET

  world.position[e] = position
  world.velocity[e] = velocity

  world.renderable[e] = renderBullet

  var s = 6
  world.boundingBox[e] = {x: position.x - s/2,
                          y: position.y - s/2,
                          width: s, height: s}
  world.hits[e] = new Set()

  return e
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main

function loop() {
  clearInput(world)
  keyboardControl(world)
  gamepadControl(world)
  tankControl(world)
  directControl(world)
  rotator(world)
  move(world)
  bulletCannon(world)
  collisionDetection(world)
  strayEntitiesCollector(world)

  for (var e1 = 0, n = world.mask.length; e1 < n; ++e1) {
    if (world.mask[e1] & C_ASTEROID) {
      var h = world.hits[e1]
      for (var e2 of h) {
        if (world.mask[e2] & C_BULLET) {
          destroyEntity(world, e2)
          fragmentAsteroid(e1)
        }
      }
    }
  }

  render(world, ctx)

  if (debug) drawDebug(ctx)

  requestAnimationFrame(loop)
}

var ship = null

function init() {
  ship = createShip(world, 15, 15)
  world.mask[ship] |= C_INPUT_GAMEPAD | C_INPUT_KEYBOARD

  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)
  createAsteroid(world,
                 Math.random() * world.width,
                 Math.random() * world.height)

  loop()
}

document.addEventListener('DOMContentLoaded', init)


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Helpers


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Debugging

document.addEventListener('DOMContentLoaded', initGUI)

var activateDirectControl = false
var debug = true
var cameraType = 'fixed'

function initGUI() {
  var gui = new dat.GUI()

  gui.add(window, 'acceleration', -1, 1)
  gui.add(window, 'angularSpeed', 0, 1)
  gui.add(window, 'bulletInitialSpeed', 0, 20)
  gui.add(window, 'activateDirectControl').onChange(function(value) {
    if (value) {
      world.mask[ship] &= ~C_TANK_CONTROL
      world.mask[ship] |= C_DIRECT_CONTROL
    }
    else {
      world.mask[ship] &= ~C_DIRECT_CONTROL
      world.mask[ship] |= C_TANK_CONTROL
    }
  })
  gui.add(window, 'directControlSpeed', 0, 20)
  gui.add(window, 'minAsteroidSize', 10, 100)
  gui.add(window, 'maxCannonHeat', 1, 100)
  gui.add(window, 'cameraType', ['fixed', 'centered', 'ahead']).onChange(function(value) {
    if (value === 'fixed')
      camera = fixedCamera
    else if (value === 'centered')
      camera = focusedCamera.new(ship)
  })
  gui.add(window, 'debug')
}

function countEntities() {
  var c = 0
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if (world.mask[e] !== C_NONE && world.mask[e] != null) {
      ++c
    }
  }
  return c
}

function drawDebug(ctx) {
  ctx.fillText('#entities: ' + countEntities(), 5, 10)

  ctx.save()
  ctx.translate(camera.vec().x, camera.vec().y)

  drawBoundingBox(ctx)
  drawAcceleration(ctx)
  drawGrid(ctx, spatialHashCellSize, spatialHashCellSize)
  drawSpatialHashInfo(ctx)

  ctx.restore()
}

function drawBoundingBox(ctx) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_BOUNDING_BOX)
        === C_BOUNDING_BOX) {
      var b = world.boundingBox[e]
      var h = world.hits[e]
      if (h.size > 0)
        ctx.strokeStyle = '#1c1'
      else
        ctx.strokeStyle = '#c11'

      ctx.strokeRect(b.x, b.y, b.width, b.height)
    }
  }
}

function drawAcceleration(ctx) {
  var p = world.position[ship]
  var v = world.velocity[ship]

  ctx.save()

  ctx.strokeStyle = 'orange'
  ctx.translate(p.x, p.y)
  ctx.scale(2,2)

  ctx.beginPath()
  ctx.moveTo(0, 0)
  ctx.lineTo(v.x, v.y)
  ctx.stroke()

  ctx.restore()
}

function drawGrid(ctx, xstep, ystep) {
  var w = ctx.canvas.width
  var h = ctx.canvas.height

  ctx.strokeStyle = '#ccc'

  for (var x=0; x < w; x += xstep) {
    ctx.beginPath()
    ctx.moveTo(x, 0)
    ctx.lineTo(x, h)
    ctx.stroke()
  }

  for (var y=0; y < h; y += ystep) {
    ctx.beginPath()
    ctx.moveTo(0, y)
    ctx.lineTo(w, y)
    ctx.stroke()
  }
}

function drawSpatialHashInfo(ctx) {
  ctx.fillStyle = '#ccc'

  for (var kv of grid.map) {
    var cell = kv[0].split('%')
    var objs = kv[1]
    var x = parseInt(cell[0], 10) * spatialHashCellSize
    var y = parseInt(cell[1], 10) * spatialHashCellSize
    ctx.fillText(`${x},${y}`, x + 2, y + 11)
    ctx.fillText(objs.size, x + 2, y + 21)
  }

}

function printKey() {
  function print(ev) {
    console.info('keyCode: %s, which: %s', ev.keyCode, ev.which, ev)
    window.removeEventListener('keydown', print)
  }
  window.addEventListener('keydown', print)
}
