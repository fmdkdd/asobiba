document.addEventListener('DOMContentLoaded', init)

var logic_time = 20 //ms

var jump_accel = 16
var move_accel = 8
var boost_accel = 14
var friction = 3
var mass = 8
var gravity = 8

var floorY = 150

var jumpingFrames = 10
var canJump = jumpingFrames
var jumping = false

var canvas
var ctx

function init() {
  canvas = document.getElementById('canvas')
  ctx = canvas.getContext('2d')

  canvas.height = 640
  canvas.width = 480

  window.addEventListener('keydown', keydown)
  window.addEventListener('keyup', keyup)

  loop(0)
}

var keys = []
const K_LEFT  = 37,
      K_UP    = 38,
      K_RIGHT = 39,
      K_DOWN  = 40,
      K_A     = 65

function keydown(ev) {keys[ev.which] = true}
function keyup(ev) {keys[ev.which] = false}

var blob = {
  pos: {x: 50, y: floorY},
  vel: {x: 0, y: 0},

  computeForces() {
    var forces = {x: 0, y: 0}

    // Impulse from input
    if (input[I_MOVE] !== 0) {
      if (input[I_BOOST])
        forces.x += input[I_MOVE] * boost_accel
      else
        forces.x += input[I_MOVE] * move_accel
    }

    if (input[I_JUMP])
      forces.y -= jump_accel

    var d = floorY - this.pos.y
    if (d > 0)
      forces.y += gravity

    // Friction to cap speed
    // This one is floaty
    // var v = this.vel.x * this.vel.x + this.vel.y * this.vel.y
    // This one is tighter
    var v = {x: Math.abs(this.vel.x), y: Math.abs(this.vel.y)}
    forces.x += -1 * Math.sign(this.vel.x) * v.x * friction
    // forces.y += -1 * Math.sign(this.vel.y) * v.y * friction
    // This one is the tightest (no inertia at all)
    // forces.x += -this.vel.x * move_accel

    return forces
  },

  update() {
    // Symplectic Euler integration
    // see: http://gamedev.stackexchange.com/questions/15708/how-can-i-implement-gravity/16466#16466
    var forces = this.computeForces()

    var accel = {x: forces.x / mass, y: forces.y / mass}

    this.vel.x += accel.x
    this.vel.y += accel.y

    this.pos.x += this.vel.x
    this.pos.y += this.vel.y
  },

  updateVerlet(dt) {
    // Velocity Verlet method
    // see: http://gamedev.stackexchange.com/questions/15708/how-can-i-implement-gravity/16466#16466
    var forces = this.computeForces()

    var accel = {x: forces.x / mass, y: forces.y / mass}

    this.pos.x += this.vel.x + accel.x / 2
    this.pos.y += this.vel.y + accel.y / 2

    forces = this.computeForces()
    var newAccel = {x: forces.x / mass, y: forces.y / mass}

    this.vel.x += (accel.x + newAccel.x) / 2
    this.vel.y += (accel.y + newAccel.y) / 2
  },

  checkCollisions() {
    if (this.pos.y >= floorY) {
      this.pos.y = floorY
      this.vel.y = 0
      canJump = jumpingFrames
      jumping = false;
    }
  },
}

var lastFrameTime = 0
var leftOver = 0

var input = []
const I_MOVE  = 0,
      I_JUMP  = 1,
      I_BOOST = 2

function loop(now) {
  var dt = now - lastFrameTime
  lastFrameTime = now

  // var dt = 1000/20

  // Input
  input[I_JUMP] = 0
  input[I_MOVE] = 0
  input[I_BOOST] = 0
  if (keys[K_RIGHT]) {input[I_MOVE] = 1}
  if (keys[K_LEFT]) {input[I_MOVE] = -1}
  if (keys[K_A]) {input[I_BOOST] = 1}
  if (keys[K_UP] && canJump) {input[I_JUMP] = 1; jumping = true}
  if (!keys[K_UP] && jumping) {canJump = 0}

  // Fixed-step logic update
  leftOver += dt
  while (leftOver > logic_time) {
    blob.updateVerlet()
    blob.checkCollisions()

    if (canJump)
      --canJump

    leftOver -= logic_time
  }

  // Render
  ctx.clearRect(0,0, canvas.width, canvas.height)

  if (canJump)
    ctx.fillStyle = 'hsl(120,60%,60%)'
  else
    ctx.fillStyle = 'hsl(0,60%,60%)'
  ctx.beginPath()
  ctx.moveTo(Math.floor(blob.pos.x), Math.floor(blob.pos.y))
  ctx.lineTo(Math.floor(blob.pos.x - blob.vel.x), Math.floor(blob.pos.y - 20))
  ctx.lineTo(Math.floor(blob.pos.x - blob.vel.x + 10), Math.floor(blob.pos.y - 20))
  ctx.lineTo(Math.floor(blob.pos.x + 10), Math.floor(blob.pos.y))
  ctx.fill()

  ctx.strokeStyle = 'black'
  ctx.beginPath()
  ctx.moveTo(0,floorY)
  ctx.lineTo(100,floorY)
  ctx.stroke()

  // Next loop
  // setTimeout(loop, dt)
  requestAnimationFrame(loop)
}
