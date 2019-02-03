document.addEventListener('DOMContentLoaded', start)

async function start() {
  const ws = new WebSocket('ws://localhost:8080')
  await onOpen(ws)
  ws.onclose = () => {
    console.log('WebSocket closed')
  }

  console.log('Creating new RTCPeerConnection')
  const pc = new RTCPeerConnection()

  let dc
  pc.ondatachannel = ev => {
    console.log('Got data channel')
    ws.close()
    ev.channel.onopen = () => {
      console.log('Data channel open')
      ev.channel.send('Hello')
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
