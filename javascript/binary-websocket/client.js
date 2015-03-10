var WebSocket = new require('ws')
var ws = new WebSocket('ws://localhost:8081', {perMessageDeflate: true})

ws.on('open', function open() {
  console.log('Connected to server, supports:', ws.supports)

  var msg = new Float64Array(1)
  msg[0] = Math.PI

  // Without compress:false, raises "TypeError: Invalid
  // non-string/buffer chunk".
  // binary:true is not needed, but it doesn’t hurt to be explicit.
  ws.send(msg, {binary: true, compress: false})

 // Payload (inspected on the wire) is 8 bytes.
 // Total packet size is 80 bytes

  // Now sending the exact same number as a string
  msg = JSON.stringify(Math.PI)

  ws.send(msg, {binary: false, compress:true})
  // String is 17 chars, payload is 24 bytes.

  ws.send(msg, {binary: false, compress:false})
  // Payload is 17 bytes.

  msg = 'blablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablablabla'

  ws.send(msg, {binary: false, compress:true})
  // String is 1008 bytes, payload is 18 bytes

  ws.send(msg, {binary: false, compress:false})
  // Payload is 1008 bytes

  // 1 Float64 + 1 Uint32 = 12 bytes
  var buff = new ArrayBuffer(12)
  var f = new Float64Array(buff, 0, 1)
  f[0] = 0xfeefbeebdeed
  var d = new Uint32Array(buff, 8, 1)
  d[0] = 0xdeadbeef

  console.log(f[0], d[0])
  ws.send(buff, {binary: true, compress: false})

  function afterSend() {
    console.log('message sent', msg) } })

ws.on('message', function(data, flags) {
  console.log('received:', data, data.length, flags) })
