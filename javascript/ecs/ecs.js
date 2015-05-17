// Trying out the ECS pattern as described here:
// http://www.gamedev.net/page/resources/_/technical/game-programming/implementing-component-entity-systems-r3382

// TODO: time-based updates rather than discrete increments.  Gameplay becomes
// independent of frame rate.
// TODO: DatGUI for changing the variables
// TODO: collision detection

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Engine

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
    C_DESTROY_OUT_OF_VIEW = 0b10000000000

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
}

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

function bulletCannon(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & bulletCannonMask) === bulletCannonMask) {
      var input = world.input[e]

      if (input[I_FIRE]) {
        var p = world.position[e]
        var r = world.rotation[e]

        var bulletPosition = {
          x: p.x + (10 * Math.cos(r)),
          y: p.y + (10 * Math.sin(r))
        }

        var v = world.velocity[e]
        var speed = Math.sqrt(v.x * v.x + v.y * v.y)
        var bulletSpeed = speed + 5
        var bulletVelocity = {
          x: bulletSpeed * Math.cos(r),
          y: bulletSpeed * Math.sin(r)
        }

        createBullet(world, bulletPosition, bulletVelocity)
      }
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Engine

var strayEntitiesCollectorMask = C_DESTROY_OUT_OF_VIEW | C_POSITION

function strayEntitiesCollector(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & strayEntitiesCollectorMask)
        === strayEntitiesCollectorMask) {
      var p = world.position[e]

      if (p.x < 0 || p.x > world.width || p.y < 0 || p.y > world.height)
        destroyEntity(world, e)
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

function keyboardControl(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_INPUT_KEYBOARD) === C_INPUT_KEYBOARD) {
      var input = world.input[e]

      // Clear input
      for (var k in input) input[k] = false

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

      // Clear input
      for (var k in input) input[k] = false

      if (axes[0] < -AXIS_THRESHOLD) input[I_ROTATE_LEFT] = true
      if (axes[0] > AXIS_THRESHOLD) input[I_ROTATE_RIGHT] = true
      if (buttons[6].pressed) input[I_THRUST] = true
      if (buttons[0].pressed) input[I_FIRE] = true
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Rendering

var ctx

function render(world, ctx) {
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & C_RENDERABLE) === C_RENDERABLE) {
      world.renderable[e](e, ctx)
    }
  }
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

  ctx.fillStyle = 'hsl(180,70%,30%)'
  ctx.translate(x, y)
  ctx.scale(3,3)

  ctx.beginPath()
  ctx.arc(0, 0, 1, 0, Math.PI * 2)
  ctx.fill()

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

  world.position[e] = {x, y}
  world.velocity[e] = {x: 0, y: 0}
  world.rotation[e] = 0
  world.input[e] = Object.create(null)

  world.renderable[e] = renderShip

  return e
}

function createAsteroid(world, x, y) {
  var e = createEntity(world)

  world.mask[e] = C_POSITION
    | C_VELOCITY
    | C_ROTATION
    | C_ROTATOR
    | C_RENDERABLE

  world.position[e] = {x, y}
  world.velocity[e] = {x: 0, y: 0}
  world.rotation[e] = 0

  world.size[e] = 40 + Math.random() * 100
  world.rotatorSpeed[e] = 1 / world.size[e]

  world.shape[e] = []
  for (var i = 0, a = 0; i < 6; ++i, a += Math.PI /3) {
    var m = 0.4 + Math.random()/3
    world.shape[e].push({x: m* Math.cos(a), y: m * Math.sin(a)})
  }

  world.renderable[e] = renderAsteroid

  return e
}


function createBullet(world, position, velocity) {
  var e = createEntity(world)

  world.mask[e] = C_POSITION
    | C_VELOCITY
    | C_RENDERABLE
    | C_DESTROY_OUT_OF_VIEW

  world.position[e] = position
  world.velocity[e] = velocity

  world.renderable[e] = renderBullet

  return e
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main

function loop() {
  keyboardControl(world)
  gamepadControl(world)
  tankControl(world)
  rotator(world)
  move(world)
  bulletCannon(world)
  strayEntitiesCollector(world)

  render(world, ctx)

  requestAnimationFrame(loop)
}

function init() {
  var s = createShip(world, 15, 15)
  world.mask[s] |= C_INPUT_GAMEPAD

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

function printKey() {
  function print(ev) {
    console.info('keyCode: %s, which: %s', ev.keyCode, ev.which, ev)
    window.removeEventListener('keydown', print)
  }
  window.addEventListener('keydown', print)
}
