var WebSocketServer = require('ws').Server
var wsserver = new WebSocketServer({port: 8081})

wsserver.on('connection', function connection(ws) {
  console.log('Connection open')

  ws.on('message', function incoming(message, flags) {
    console.log('received:', message, message.length, flags)

    if (flags.binary)
      console.log(message.readDoubleLE(0))
    else
      console.log(message)

    ws.send(message) })})
