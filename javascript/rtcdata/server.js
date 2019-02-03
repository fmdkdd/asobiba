const express = require('express')
const http = require('http')
const ws = require('ws')
const wrtc = require('wrtc')

const app = express()
app.use(express.static(__dirname))

const httpServer = http.createServer(app)
httpServer.listen(8080, () => {
  const address = httpServer.address()
  console.log(`HTTP server started at http://${address.address}:${address.port}`)
})

new ws.Server({server: httpServer}).on('connection', async ws => {
  console.log('Creating new RTCPeerConnection')

  const pc = new wrtc.RTCPeerConnection()
  pc.onicecandidate = ev => {
    if (ev.candidate) {
      console.log('Sending ICE candidate')
      ws.send(JSON.stringify({
        type: 'candidate',
        candidate: ev.candidate,
      }))
    } else {
      console.log('ICE gathering complete')
    }
  }

  const dc = pc.createDataChannel('updates', {
    ordered: false,
    maxRetransmits: 0,
  })

  dc.onopen = () => {
    console.log('Data channel open')
    start(pc, dc)
  }

  dc.onclose = () => {
    console.log('Data channel closed')
  }

  console.log('Sending offer')
  const offer = await pc.createOffer()
  await pc.setLocalDescription(offer)
  ws.send(JSON.stringify({
    type: 'offer',
    offer,
  }))

  ws.on('message', async msg => {
    const m = JSON.parse(msg)
    switch (m.type) {
    case 'answer':
      console.log('Got answer')
      await pc.setRemoteDescription(m.answer)
      ws.close()
      break

    case 'candidate':
      console.log('Adding ICE candidate')
      await pc.addIceCandidate(m.candidate)
      break

    default:
      console.log('Unknown message', m.type)
    }
  })

  ws.on('close', () => {
    console.log('WebSocket closed')
  })
})

function start(pc, dc) {
  const UP    = 1
  const DOWN  = 2
  const LEFT  = 4
  const RIGHT = 8

  const player = {
    x: 100,
    y: 100,
    input: 0
  }

  let latched_input = 0
  dc.onmessage = msg => {
    player.input = msg.data
    latched_input |= player.input
  }

  let last_frame = process.hrtime.bigint()

  loop()

  function loop() {
    const now = process.hrtime.bigint()
    const dt = Number((now - last_frame) / 1000000n)
    last_frame = now

    // TODO: Problem with polling inputs.  If the server updates at a low
    // frequency, we may miss short key presses from the player.  Latching
    // inputs ensures we don't miss them, but then we play them for too long: a
    // 100ms press is interpreted as a 1s press.  We need to know how long the
    // key press was in order to correctly interpret it server-side.

    // Poll input and update state
    const speed = .2

    if (latched_input & RIGHT)
      player.x += speed * dt
    if (latched_input & LEFT)
      player.x -= speed * dt
    if (latched_input & UP)
      player.y -= speed * dt
    if (latched_input & DOWN)
      player.y += speed * dt

    latched_input = player.input

    // Send state to client
    dc.send(JSON.stringify({
      x: Math.round(player.x),
      y: Math.round(player.y),
    }))

    setTimeout(loop, 1000)
  }
}
