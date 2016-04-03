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

  server.send(inputMask)

  lastInputMask = inputMask
}

function init() {
  var lastInputTime = 0

  // Input
  document.addEventListener('keydown', function(ev) {
    keyMask[ev.which] = true
  })
  document.addEventListener('keyup', function(ev) {
    keyMask[ev.which] = false
  })

  // Websocket
  server = new WebSocket('ws://localhost:12345')

  server.addEventListener('open', function() {
    console.info('Connected to websocket server')

    // Start sending input
    // sendInput()
    loop()
  })

  server.addEventListener('close', function(close) {
    console.info('Disconnected from websocket server', close)
  })

  server.addEventListener('error', function(err) {
    console.error('Error with websocket connection', err)
  })

  server.addEventListener('message', function(msg) {
    // console.log('Message from websocket', msg)
    var reader = new FileReader()
    reader.addEventListener('loadend', function() {
      var x = new DataView(reader.result).getInt32(0, false)
      var y = new DataView(reader.result).getInt32(4, false)

      ship.x = x
      ship.y = y
    })
    reader.readAsArrayBuffer(msg.data)
  })

  // Rendering
  canvas = document.querySelector('#screen')
  ctxt = canvas.getContext('2d')

  window.addEventListener('resize', resizeCanvas)
  resizeCanvas()
}

function resizeCanvas() {
  canvas.width = window.innerWidth
  canvas.height = window.innerHeight
  // This also works:
  // canvas.width = document.body.clientWidth
  // canvas.height = document.body.clientHeight
}

function loop() {
  sendInput()
  render()

  requestAnimationFrame(loop)
}

var ship = {x: 0, y: 0}

function render() {
  ctxt.fillStyle = 'black'
  ctxt.fillRect(0, 0, canvas.width, canvas.height)

  ctxt.fillStyle = 'white'
  ctxt.fillRect(ship.x, -ship.y, 10, 10)

}
