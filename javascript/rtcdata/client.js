document.addEventListener('DOMContentLoaded', init)

async function init() {
  const ws = new WebSocket('ws://localhost:8080')
  await onOpen(ws)
  ws.onclose = () => {
    console.log('WebSocket closed')
  }

  console.log('Creating new RTCPeerConnection')
  const pc = new RTCPeerConnection()

  pc.ondatachannel = ev => {
    console.log('Got data channel')
    ws.close()
    ev.channel.onopen = () => {
      console.log('Data channel open')
      start(pc, ev.channel)
    }
  }

  ws.onmessage = async msg => {
    const m = JSON.parse(msg.data)
    switch (m.type) {
    case 'offer':
      console.log('Got offer, sending answer')
      await pc.setRemoteDescription(m.offer)
      const answer = await pc.createAnswer()
      await pc.setLocalDescription(answer)
      ws.send(JSON.stringify({
        type: 'answer',
        answer,
      }))
      break

    case 'candidate':
      console.log('Adding ICE candidate')
      await pc.addIceCandidate(m.candidate)
      break

    default:
      console.log('Unknown message', m.type)
    }
  }

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
}

function onOpen(ws) {
  return new Promise((resolve, reject) => {
    ws.onopen = () => resolve();
    ws.onclose = () => reject(new Error('WebSocket closed'));
  });
}

function start(pc, dc) {
  const serverState = {
    x: 0,
    y: 0,
  }

  const clientState = {
    x: 0,
    y: 0,
  }

  dc.onmessage = msg => {
    const m = JSON.parse(msg.data)
    if (m.x) serverState.x = m.x
    if (m.y) serverState.y = m.y

    clientState.x = serverState.x
    clientState.y = serverState.y
  }

  const canvas = document.getElementById('canvas')
  canvas.width = canvas.height = 500
  const ctx = canvas.getContext('2d')

  const UP    = 1
  const DOWN  = 2
  const LEFT  = 4
  const RIGHT = 8

  let input = 0

  window.addEventListener('keydown', ev => {
    switch (ev.which) {
    case 87: input |= UP;    break
    case 82: input |= DOWN;  break
    case 65: input |= LEFT;  break
    case 83: input |= RIGHT; break
    }

  })

  window.addEventListener('keyup', ev => {
    switch (ev.which) {
    case 87: input &= ~UP; break
    case 82: input &= ~DOWN; break
    case 65: input &= ~LEFT; break
    case 83: input &= ~RIGHT; break
    }
  })

  let last_frame = 0

  loop()

  function loop(now) {
    const dt = now - last_frame
    last_frame = now

    // Send input
    dc.send(input)

    // Client-side prediction
    const speed = .2

    if (input & RIGHT)
      clientState.x += speed * dt
    if (input & LEFT)
      clientState.x -= speed * dt
    if (input & UP)
      clientState.y -= speed * dt
    if (input & DOWN)
      clientState.y += speed * dt

    // Redraw
    ctx.clearRect(0,0, 500, 500)
    ctx.lineWidth = 6
    ctx.strokeStyle = 'red'
    ctx.strokeRect(serverState.x, serverState.y, 50, 50)
    ctx.strokeStyle = 'green'
    ctx.strokeRect(clientState.x, clientState.y, 50, 50)

    requestAnimationFrame(loop)
  }
}
