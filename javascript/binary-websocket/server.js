var WebSocketServer = require('ws').Server
var wsserver = new WebSocketServer({port: 8081
                                    ,perMessageDeflate: true })

wsserver.on('connection', function connection(ws) {
  console.log('Connection open')

  ws.on('message', function incoming(message, flags) {
    console.log('received:', message, message.length, flags)

    if (flags.binary) {
      console.log(message.readDoubleLE(0))
      if (message.length > 8)
        console.log(message.readUInt32LE(8))
    }
    else
      console.log(message)

    ws.send(message) })})
