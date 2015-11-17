document.addEventListener('DOMContentLoaded', init)

var canvas, ctx
var world, central, ground, left, right, target
var leftConstraint, rightConstraint

var settings = {
  gravity: 9.82,
  thrust: 0.4,
}

function init() {
  canvas = document.getElementById('canvas')
  ctx = canvas.getContext('2d')

  canvas.width = 600
  canvas.height = 400
  ctx.lineWidth = 0.05

  world = new p2.World({
    gravity: [0, -settings.gravity]
  })

  central = new p2.Body({
    mass: 1,
    position: [0, 200],
  })

  central.addShape(new p2.Box({
    height: 2,
    width: 1
  }))

  world.addBody(central)

  left = new p2.Body({
    mass: 1,
    position: [-1, 199]
  })

  left.addShape(new p2.Box({
    height: 2,
    width: 1
  }))
  world.addBody(left)

  leftConstraint = new p2.LockConstraint(left, central)
  world.addConstraint(leftConstraint)

  right = new p2.Body({
    mass: 1,
    position: [1, 199]
  })

  right.addShape(new p2.Box({
    height: 2,
    width: 1
  }))
  world.addBody(right)

  rightConstraint = new p2.LockConstraint(central, right)
  world.addConstraint(rightConstraint)

  var heights = [5, 2, 1, 0, 0, 0, 1, 4, 0]

  ground = new p2.Body({
    position: [-20,-1]
  })
  ground.addShape(new p2.Heightfield({
    heights: heights,
    elementWidth: 5
  }))
  world.addBody(ground)

  target = new p2.Body({
    position: [-5,-0.9]
  })
  target.addShape(new p2.Heightfield({
    heights: [0,0],
    elementWidth: 10
  }))
  world.addBody(target)

  world.on('beginContact', beginContact)
  world.on('endContact', endContact)

  // var gui = new dat.GUI()
  // gui.add(settings, 'gravity', 1, 100).onChange(function() {
  //   world.gravity[1] = -settings.gravity
  // })

  loop()
}

function beginContact(ev) {
  if ((ev.bodyA === left && ev.bodyB === ground)
      || (ev.bodyA === ground && ev.bodyB === left)) {
    world.removeConstraint(leftConstraint)
  }

  else if ((ev.bodyA === right && ev.bodyB === ground)
      || (ev.bodyA === ground && ev.bodyB === right)) {
    world.removeConstraint(rightConstraint)
  }

  else if ((ev.bodyA === left && ev.bodyB === target)
           || (ev.bodyA === target && ev.bodyB === left)) {
    console.log('hurray')
  }
}

function endContact(ev) {
}

var physicsStep = 1 / 200
var lastTime = 0

var inputThrust = [0,0]

function loop(t) {
  var dt = t - lastTime

  // console.log(Object.keys(keys))

  inputThrust[0] = inputThrust[1] = 0
  keyboardControl()
  gamepadControl()

  left.thrusting = false
  right.thrusting = false
  if (inputThrust[0]) {
    thrust(left, inputThrust[0])
    left.thrusting = true
  }
  if (inputThrust[1]) {
    thrust(right, inputThrust[1])
    right.thrusting = true
  }

  world.step(physicsStep, dt / 1000)

  render(ctx)

  lastTime = t
  requestAnimationFrame(loop)
}

function keyboardControl() {
  if (keys[65])
    inputThrust[0] = 1
  if (keys[82])
    inputThrust[1] = 1
}

function gamepadControl() {
  var pads = navigator.getGamepads()
  var pad = pads[0]

  if (!pad) return

  var axes = pad.axes
  var buttons = pad.buttons

  inputThrust[0] = buttons[6].value /2
  inputThrust[1] = buttons[7].value /2
}

function thrust(body, force) {
  var impulse = [0, force]
  p2.vec2.rotate(impulse, impulse, body.angle)
  body.applyImpulse(impulse)
}

document.addEventListener('keydown', onKeyDown)
document.addEventListener('keyup', onKeyUp)

var keys = {}
function onKeyDown(event) {
  keys[event.which] = true
}

function onKeyUp(event) {
  keys[event.which] = false
}

var camera = {
  zoom: 10,
  x: 0,
  y: 0,

  focus(position) {
    camera.x = canvas.width/ 2 / camera.zoom - position[0]
    camera.y = canvas.height/ 2 / -camera.zoom - position[1]
  }
}

var shipSize = 30

function render(ctx) {
  ctx.clearRect(0,0,canvas.width,canvas.height)

  camera.focus(central.interpolatedPosition)
  var d = p2.vec2.dist(central.interpolatedPosition, target.interpolatedPosition)
  camera.zoom = canvas.height/2 / d
  camera.zoom = Math.min(camera.zoom,  shipSize)

  ctx.save()
  ctx.scale(camera.zoom, -camera.zoom)
  ctx.translate(camera.x, camera.y)

  // drawBox(ctx, central)
  // drawBox(ctx, left)
  // drawBox(ctx, right

  ctx.strokeStyle = 'black'
  drawPlane(ctx, ground)
  ctx.strokeStyle = 'blue'
  drawPlane(ctx, target)

  ctx.restore()

  ctx.save()
  ctx.translate(canvas.width/2, canvas.height/2)
  ctx.scale(shipSize,-shipSize)
  ctx.translate(-central.interpolatedPosition[0], -central.interpolatedPosition[1])
  drawBox(ctx, central)
  drawBox(ctx, left)
  drawBox(ctx, right)
  ctx.restore()
}

function drawBox(ctx, box) {
  ctx.strokeStyle = box.thrusting ? '#1c1' : '#c11'
  var x = box.interpolatedPosition[0]
  var y = box.interpolatedPosition[1]
  var s = box.shapes[0]
  ctx.save()
  ctx.translate(x, y)
  ctx.rotate(box.interpolatedAngle)
  ctx.strokeRect(-s.width/2, -s.height/2, s.width, s.height)
  ctx.restore()
}

function drawPlane(ctx, plane) {
  ctx.lineWidth = 1
  var x = plane.interpolatedPosition[0]
  var y = plane.interpolatedPosition[1]
  ctx.save()
  ctx.beginPath()
  ctx.translate(x,y)
  x = 0
  ctx.moveTo(0,0)
  for (var i = 0; i < plane.shapes[0].heights.length; ++i) {
    var h = plane.shapes[0].heights[i]
    ctx.lineTo(x,h)
    x += plane.shapes[0].elementWidth
  }
  ctx.stroke()
  ctx.restore()
}

function drawTarget(ctx, line) {
  ctx.strokeStyle = 'blue'
  var x = line.interpolatedPosition[0]
  var y = line.interpolatedPosition[1]
  var s = line.shapes[0]
  ctx.save()
  ctx.translate(x,y)
  ctx.moveTo(-s.length/2,0)
  ctx.lineTo(s.length/2,0)
  ctx.stroke()
  ctx.restore()
}
