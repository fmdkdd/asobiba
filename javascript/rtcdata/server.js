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
  }

  dc.onclose = () => {
    console.log('Data channel closed')
  }

  dc.onmessage = msg => {
    console.log('Data channel message: ', msg.data)
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
