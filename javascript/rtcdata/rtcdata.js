document.addEventListener('DOMContentLoaded', init)

async function init() {
  document.getElementById('initiate')
    .addEventListener('click', initiate)

  document.getElementById('receive')
    .addEventListener('click', receive)

    document.getElementById('accept')
    .addEventListener('click', accept)
}

const pc = new RTCPeerConnection()

pc.ondatachannel = (ev) => {
  console.log('got data channel')
}

pc.onicecandidate = (ev) => {
  console.log('ice candidate', ev)
  pc.addIceCandidate(ev.candidate)
}

pc.oniceconnectionstatechange = (ev) => {
  console.log('ice statechange', ev)
}

async function initiate() {
  const dc = pc.createDataChannel('data', {
    ordered: false,
    maxRetransmits: 0,
  })

  dc.onopen = ev => {
    console.log('data channel open')
  }

  dc.onmessage = (ev) => {
    console.log('data message', ev.data)
  }

  dc.onclose = (ev) => {
    console.log('data channel closed')
  }

  dc.onerror = (ev) => {
    console.log('data channel error')
  }

  const offer = await pc.createOffer()
  pc.setLocalDescription(offer)

  document.getElementById('sdp')
    .value = JSON.stringify(offer)
}

async function receive() {
  const sdp = JSON.parse(document.getElementById('sdp').value)

  await pc.setRemoteDescription(sdp)
  const answer = await pc.createAnswer()
  pc.setLocalDescription(answer)
  document.getElementById('sdp').value = JSON.stringify(answer)
}

async function accept() {
  const sdp = JSON.parse(document.getElementById('sdp').value)
  await pc.setRemoteDescription(sdp)
}
