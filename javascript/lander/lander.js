
// [[file:lander.org::*Components][Components:1]]

var C_NONE         = 0b0,
    C_BOUNDING_BOX = 0b1,
    C_POSITION     = 0b10,
    C_FORCES       = 0b100,
    C_GRAVITY_PULL = 0b1000,
    C_THRUST_LEFT  = 0b10000,
    C_CONSTRAINT   = 0b100000

// World state
var world = {
  input: Object.create(null),
  mask: [],
  boundingBox: [],
  boundingBoxHit: [],
  position: [],
  velocity: [],
  forces: [],
  constraint: [],
}

// Entity creation and destruction
function createEntity(world) {
  var i = 0
  while (world.mask[i] !== C_NONE && world.mask[i] != null)
    ++i

  return i
}

function destroyEntity(world, e) {
  world.mask[e] = C_NONE
}

// Components:1 ends here

// [[file:lander.org::*Entities][Entities:1]]

function createLander(world, x, y) {
  var body = createBody(world, x, y)
  var left = createLeftEngine(world, body)
  // var right = createLeftEngine(world, body)

  world.mask[body] |= C_CONSTRAINT
  world.constraint[body] = left
}

function createLeftEngine(world, body) {
  var x = world.position[body].x - 50
  var y = world.position[body].y

  var e = createEntity(world)

  world.mask[e] = C_BOUNDING_BOX
    | C_POSITION
    | C_FORCES
    | C_GRAVITY_PULL
    | C_THRUST_LEFT
    | C_CONSTRAINT

  world.position[e] = vector(x, y)
  world.velocity[e] = vector(0, 0)
  world.forces[e] = []
  world.constraint[e] = body

  var size = 50
  world.boundingBox[e] = {x: x - size/2, y: y - size/2,
                          width: size, height: size}

  world.boundingBoxHit[e] = false

  return e
}

function createBody(world, x, y) {
  var e = createEntity(world)

  world.mask[e] = C_BOUNDING_BOX
    | C_POSITION
    | C_FORCES
    | C_GRAVITY_PULL

  world.position[e] = vector(x, y)
  world.velocity[e] = vector(0, 0)
  world.forces[e] = []

  var size = 50
  world.boundingBox[e] = {x: x - size/2, y: y - size/2,
                          width: size, height: size}

  world.boundingBoxHit[e] = false

  return e
}

// Entities:1 ends here

// [[file:lander.org::*Forces][Forces:1]]

var forceMask = C_POSITION | C_FORCES

function applyForces(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & forceMask) === forceMask) {
      var v = world.velocity[e]
      var fs = world.forces[e]
      var f
      while (fs.length) {
        f = fs.pop()
        v.x += f.x
        v.y += f.y
      }

      var p = world.position[e]
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

// Forces:1 ends here

// [[file:lander.org::*Thrusting][Thrusting:1]]

var thrustMask = C_FORCES | C_THRUST_LEFT
var thrustPower = 0.050

function thrusting(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & thrustMask) === thrustMask) {
      var fs = world.forces[e]
      var v = vector(0,0)

      if (world.input[I_THRUST_LEFT]) v.y = -thrustPower

      fs.push(v)
    }
  }
}

// Thrusting:1 ends here

// [[file:lander.org::*Gravity%20pull][Gravity\ pull:1]]

var gravityMask = C_GRAVITY_PULL | C_POSITION | C_FORCES

var G = 0.025
var planetMass = 100
var landerMass = 1
var distMult = 1
var dampening = 300

function gravityPull(world) {
  var floor = point(0, world.height)

  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & gravityMask) === gravityMask) {
      var p = world.position[e]
      var fs = world.forces[e]

      var dist = vec_length(vec_minus(floor, point(0, p.y)))
      // var force = G * (landerMass * planetMass) / Math.pow(dist, 2)

      var force = G / Math.pow(dist / dampening, 2)

      var v = vector(0, force)

      fs.push(v)
    }
  }
}

// Gravity\ pull:1 ends here

// [[file:lander.org::*Body%20constraints][Body\ constraints:1]]

var bodyConstraintMask = C_POSITION | C_FORCES | C_CONSTRAINT
var K = 0.01

function bodyConstraints(world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & bodyConstraintMask) === bodyConstraintMask) {
      var p = world.position[e]
      var other = world.constraint[e]
      var op = world.position[other]
      var fs = world.forces[e]

      var d = vec_minus(op, p)
      var dist = vec_length(d)

      var collide = do_boxes_collide(world.boundingBox[0], world.boundingBox[1])
      world.boundingBoxHit[0] = collide
      world.boundingBoxHit[1] = collide

      var force = vec_mult(d, K)
      if (!collide) {
        fs.push(force)
      } else {
        fs.push(vec_mult(force, -.5))
      }
    }
  }
}

// Body\ constraints:1 ends here

// [[file:lander.org::*Input][Input:1]]

var keys = Object.create(null)

var K_LEFT  = 37,
    K_UP    = 38,
    K_RIGHT = 39,
    K_SPACE = 32

function onKeyDown(ev) { keys[ev.which] = true }
function onKeyUp(ev) { keys[ev.which] = false }

var I_THRUST_LEFT  = 0,
    I_THRUST_RIGHT = 1

function keyboardControl(world) {
  // Clear input
  for (var k in world.input)
    world.input[k] = false

  if (keys[K_LEFT]) world.input[I_THRUST_RIGHT] = true
  if (keys[K_RIGHT]) world.input[I_THRUST_LEFT] = true
}

function initKeyListeners() {
  window.addEventListener('keydown', onKeyDown)
  window.addEventListener('keyup', onKeyUp)
}

// Input:1 ends here

// [[file:lander.org::*Camera][Camera:1]]

var camera = {x: 0, y: 0}

// Camera:1 ends here

// [[file:lander.org::*Rendering][Rendering:1]]

var ctx

function initCanvas() {
  var canvas = document.getElementById('canvas')
  canvas.width = window.innerWidth - 5
  canvas.height = window.innerHeight - 5
  ctx = canvas.getContext('2d')

  world.width = canvas.width
  world.height = canvas.height
}

function render(ctx) {
  ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)

  ctx.save()
  ctx.translate(camera.x, camera.y)

  ctx.restore()
}

function renderLander(ctx, lander) {}

// Rendering:1 ends here

// [[file:lander.org::*Init][Init:1]]

function init() {
  initCanvas()
  initKeyListeners()

  createLander(world, 200, 100)

  // addCollisionHandler(C_ASTEROID, C_BULLET, function(e1, e2) {
  //   destroyEntity(world, e2)
  //   fragmentAsteroid(e1)
  // })

  loop()
}

document.addEventListener('DOMContentLoaded', init)

// Init:1 ends here

// [[file:lander.org::*Game%20loop][Game\ loop:1]]

var requestFrameId

function loop() {
  keyboardControl(world)
  gravityPull(world)
  thrusting(world)
  bodyConstraints(world)

  render(ctx)

  if (debug) drawDebug(ctx, world)

  applyForces(world)

  // collisionDetection(world)
  // resolveCollisions(world)
  // strayEntitiesCollector(world)



  requestFrameId = requestAnimationFrame(loop)
}

// Game\ loop:1 ends here

// [[file:lander.org::*Game%20loop][Game\ loop:1]]

function stop() {
  cancelAnimationFrame(requestFrameId)
}

// Game\ loop:1 ends here

// [[file:lander.org::*Constants][Constants:1]]

var debug = true

function initGUI() {
  var gui = new dat.GUI()

  // gui.add(window, 'planetMass', 1, 10000)
  // gui.add(window, 'landerMass', 1, 30)
  gui.add(window, 'G', 0, 0.02)
  gui.add(window, 'dampening', 10, 1000)
  gui.add(window, 'thrustPower', 0, 1)
  gui.add(window, 'K', 0, 1)
  gui.add(window, 'debug')
}

document.addEventListener('DOMContentLoaded', initGUI)

// Constants:1 ends here

// [[file:lander.org::*Rendering][Rendering:1]]

function drawDebug(ctx, world) {
  ctx.save()
  ctx.translate(camera.x, camera.y)

  drawBoundingBox(ctx, world)
  // drawHitBox()
  drawVelocity(ctx, world)
  // drawGrid(ctx, spatialHashCellSize, spatialHashCellSize)
  // drawSpatialHashInfo(ctx)

  ctx.restore()
}

function drawBoundingBox(ctx, world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if (world.mask[e] & C_BOUNDING_BOX) {
      var b = world.boundingBox[e]
      var h = world.boundingBoxHit[e]

      ctx.strokeStyle = h ? '#1c1' : '#c11'
      ctx.strokeRect(b.x, b.y, b.width, b.height)
    }
  }
}

var drawVelocityMask = C_POSITION | C_FORCES

function drawVelocity(ctx, world) {
  for (var e = 0, n = world.mask.length; e < n; ++e) {
    if ((world.mask[e] & drawVelocityMask) === drawVelocityMask) {
      var p = world.position[e]
      var fs = world.forces[e]

      ctx.save()

      ctx.strokeStyle = 'orange'
      ctx.lineWidth = 2
      ctx.translate(p.x, p.y)

      for (var i = 0; i < fs.length; ++i) {
        var f = fs[i]
        ctx.beginPath()
        ctx.moveTo(0, 0)
        ctx.lineTo(f.x * 500, f.y * 500)
        ctx.stroke()
      }

      ctx.restore()
    }
  }
}

// Rendering:1 ends here
