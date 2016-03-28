document.addEventListener('DOMContentLoaded', init)

var server
var canvas, ctxt

var keyMask = []
var inputSendInterval = 30  // ms

const KEY_A = 65,
      KEY_R = 82,
      KEY_S = 83,
      KEY_W = 87

const I_LEFT = 0,
      I_DOWN = 1,
      I_RIGHT = 2,
      I_UP = 3

var lastInputMask = new Uint8Array(1)
function sendInput() {
  var inputMask = new Uint8Array(1)
  inputMask[0] =
    keyMask[KEY_A] << I_LEFT
    | keyMask[KEY_R] << I_DOWN
    | keyMask[KEY_S] << I_RIGHT
    | keyMask[KEY_W] << I_UP

  if (inputMask[0] !== lastInputMask[0]) {
    console.log(keyMask, inputMask)
    server.send(inputMask)

    lastInputMask = inputMask
  }

  setTimeout(sendInput, inputSendInterval)
}

function init() {
  var lastInputTime = 0

  var delays = [10, 50, 100, 200, 400]
  var delay = delays[Math.floor(Math.random()*delays.length)]

  console.log('DELAY', delay)

  // Input
  document.addEventListener('keydown', function(ev) {
    keyMask[ev.which] = true
    // var t = performance.now()
    // var dt = t - lastInputTime
    // lastInputTime = t
    // console.log(ev.which, t, dt)
    // console.log(performance.now())
    lastInputTime = performance.now()
    // setTimeout(function() {
    //   ctxt.fillStyle = ctxt.fillStyle === '#ffc0cb' ? 'green' : '#ffc0cb'
    //   ctxt.fillRect(100, 100, 300, 300)
    //   console.log(performance.now())
    // }, delay)
  })
  document.addEventListener('keyup', function(ev) {
    keyMask[ev.which] = false
    var t = performance.now()
    var dt = t - lastInputTime
    console.log(ev.which, t, dt)
  })

  // Websocket
  server = new WebSocket('ws://localhost:12345')

  server.addEventListener('open', function() {
    console.info('Connected to websocket server')

    // Start sending input
    // sendInput()
  })

  server.addEventListener('close', function(close) {
    console.info('Disconnected from websocket server', close)
  })

  server.addEventListener('error', function(err) {
    console.error('Error with websocket connection', err)
  })

  server.addEventListener('message', function(msg) {
    console.log('Message from websocket', msg)
  })

  // Rendering
  canvas = document.querySelector('#screen')
  ctxt = canvas.getContext('2d')

  window.addEventListener('resize', resizeCanvas)
  resizeCanvas()

  // Start drawing
  // render()
}

function resizeCanvas() {
  canvas.width = window.innerWidth
  canvas.height = window.innerHeight
  // This also works:
  // canvas.width = document.body.clientWidth
  // canvas.height = document.body.clientHeight
}

function render() {
  ctxt.fillColor = 'black'
  ctxt.fillRect(0, 0, canvas.width, canvas.height)

  // ctxt.


  requestAnimationFrame(render)
}
