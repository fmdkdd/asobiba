document.addEventListener("DOMContentLoaded", init)

var audio
var sample_rate = 44100
var buffer
var buffer_length = sample_rate / 2
var buffer_write_cursor
var buffer_read_cursor

function init() {
  audio = new window.AudioContext()
  buffer = []
  buffer_write_cursor = 0
  buffer_read_cursor = 0

  var script_node = audio.createScriptProcessor(4096, 2, 2)
  script_node.onaudioprocess = function(ev) {
    var buf = ev.outputBuffer
    var i = 0
    while (buffer_read_cursor != buffer_write_cursor) {
      buf.getChannelData(0)[i] = buffer[buffer_read_cursor]
      buf.getChannelData(1)[i] = buffer[buffer_read_cursor]
      buffer_read_cursor = (buffer_read_cursor + 1) % buffer_length
      i++
    }
  }

  // for (var i=0; i < buffer_length; ++i) {
  //   buffer.getChannelData(0)[buffer_cursor] = i % 220 ? +1 : -1
  //   buffer_cursor = (buffer_cursor + 1) % buffer_length
  //   // frame++
  // }

  var source = audio.createBufferSource()
  source.buffer = null
  source.loop = true


  source.connect(script_node)
  script_node.connect(audio.destination)
  source.start()
  console.log("start")

  loop()
}

var frame = 0

function loop() {
  for (var i=0; i < 200000; ++i) {
    buffer[buffer_write_cursor] = Math.sin(220 * frame)
    buffer_write_cursor = (buffer_write_cursor + 1) % buffer_length
    frame++
  }

  requestAnimationFrame(loop)
}
