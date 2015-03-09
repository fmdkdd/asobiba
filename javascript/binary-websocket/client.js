var WebSocket = new require('ws')
var ws = new WebSocket('ws://localhost:8081')

ws.on('open', function open() {
  console.log('Connected to server, supports:', ws.supports)

  var msg = new Float64Array(1)
  msg[0] = Math.PI

  // Without compress:false, raises "TypeError: Invalid
  // non-string/buffer chunk".
  // binary:true is not needed, but it doesn’t hurt to be explicit.
  ws.send(msg, {binary: true, compress: false}, afterSend)

  // Payload is 8 bytes, as expected

  // Now sending the exact same number as a string
  msg = JSON.stringify(Math.PI)

  ws.send(msg, {binary: false, compress:true}, afterSend)
  // String is 17 chars, payload is 17 bytes.

  ws.send(msg, {binary: false, compress:false}, afterSend)
  // Still 17 bytes.  The compress option does not help, even with
  // repetitive strings.

  function afterSend() {
    console.log('message sent', msg) } })

ws.on('message', function(data, flags) {
  console.log('received:', data, data.length, flags) })
