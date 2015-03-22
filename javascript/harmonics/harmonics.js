var ctx = new AudioContext()
var volume = ctx.createGain()
volume.gain.value = 0.3
volume.connect(ctx.destination)

function O(freq = 440, amp = 1, type = 'sine') {
  var g = ctx.createGain()
  g.gain.value = amp
  g.connect(volume)

  var o = ctx.createOscillator()
  o.type = type
  o.frequency.value = freq
  o.connect(g)

  return o}

function h(n) {
  return O(440 * n, 1 / Math.pow(2, n))}

// Sine wave
//h(1).start()

// Saw wave
//;[1,2,3,4,5,6].map(h).forEach(o => o.start())

// Pulse wave
//;[1,3,5,7,9].map(h).forEach(o => o.start())

// Strange wave
//;[1,1.2,1.5,3,7].map(h).forEach(o => o.start())

function arp(freqs, duration = 1000) {
  var os = []
  freqs.forEach(f => os.push(h(f), h(3.05*f), h(5.1*f)))
  return {duration
          ,start() { os.forEach(o => o.start()) }
          ,stop()  { os.forEach(o => o.stop()) }}}

function silence(duration) {
  return {duration, start: () => {}, stop: () => {}}}

function play(/* notes */) {
  var notes = Array.from(arguments)
  if (notes.length > 0) {
    var [n, ...rest] = notes
    n.start()
    setTimeout(() => { n.stop(); play(...rest) }, n.duration)}}

// Chord progression
play(arp([1, 1.2, 1.5])
     ,arp([1, 1.4, 1.7])
     ,arp([1, 1.5, 2]))
