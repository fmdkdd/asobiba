document.addEventListener('DOMContentLoaded', init)

function init() {
  var PeerConnection = window.RTCPeerConnection
        || window.webkitRTCPeerConnection

  // For local IP addresses
  var ice = null

  // For finding the public IP address
  // var ice = {"iceServers": [
  //   {"url": "stun:stun.l.google.com:19302"},
  // ]};

  var pc = new PeerConnection(ice)

  pc.onaddstream = function() {
    console.log('new data stream?')
  }

  pc.ondatachannel = function() {
    console.log('got data channel')
  }

  pc.onicecandidate = function(ev) {
    if (ev.candidate != null)
      remotepc.addIceCandidate(ev.candidate)
  }

  pc.oniceconnectionstatechange = function(ev) {
    console.log('statechange', ev)
  }

  var dc = pc.createDataChannel('data', {
    ordered: false,
    maxRetransmits: 0,
  })

  dc.onopen = function() {
    console.log('data channel open')
    // pc.getStats()
    //   .then(function(stats) { console.log(stats) })
    dc.send('bouah')
  }

  dc.onmessage = function() {
    console.log('data message')
  }

  dc.onclose = function() {
    console.log('data channel closed')
  }

  dc.onerror = function() {
    console.log('data channel error')
  }

  // Remote peer
  var remotepc = new PeerConnection(ice)

  remotepc.onicecandidate = function(ev) {
    if (ev.candidate != null)
      pc.addIceCandidate(ev.candidate)
  }

  remotepc.ondatachannel = function(ev) {
    console.log('(remote) got data channel')
    ev.channel.onmessage = function(ev) {
      console.log('(remote) data message: ', ev.data)
    }
  }

  // This spaghetti plate is brought to you by FF and Chrome not having the same
  // interface.  Chrome uses success and failure callbacks, FF a mix of Promises
  // and callbacks.
  var p = pc.createOffer(handleOffer, failure)
  if (p) p.then(handleOffer)

  function handleOffer(offer) {
    pc.setLocalDescription(offer, function() {
      remotepc.setRemoteDescription(
        pc.localDescription, function() {
          var p = remotepc.createAnswer(handleAnswer, failure)
          if (p) p.then(handleAnswer)
        }, failure)
    }, failure)
  }

  function handleAnswer(answer) {
    remotepc.setLocalDescription(
      answer, function() {
        pc.setRemoteDescription(remotepc.localDescription)
      }, failure)
  }

  function failure(err) {
    console.error(err)
  }
}
