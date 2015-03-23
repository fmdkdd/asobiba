var ctx = new AudioContext()
var volume = ctx.createGain()
volume.gain.value = 0.1
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

function H(/* fundamentals */) {
  var fundamentals = Array.from(arguments)
  var waves = fundamentals.map(f => O(440 * f, 1 / f))
  return {
    start(t = ctx.currentTime) {waves.forEach(w => w.start(t))}
    ,stop(t = ctx.currentTime) {waves.forEach(w => w.stop(t))}}}

function C(/* freqs */) {
  var fundamentals = Array.from(arguments)
  var waves = fundamentals.map(f => O(440 * f))
  return {
    start() {waves.forEach(w => w.start())}
    ,stop() {waves.forEach(w => w.stop())}}}

var channel = (f) => ({
  start() { this.sound = f()
            this.sound.start()
            this.playing = true }
  ,stop() { this.sound.stop()
            this.playing = false }
  ,toggle() { if (this.playing) this.stop()
              else this.start() }})

var acSquare = channel(_ => O(440, 1, 'square'))
var sine = channel(_ => O(440, 1, 'sine'))
var sinesSquare = channel(_ => H(1,3,5,7,9,11,13))
var sinesSaw = channel(_ => H(1,2,3,5,6,7,8,9,10))
var sinesEven = channel(_ => H(1,2,4,6,8,10))
var majorChordHarm = channel(_ => H(1,5/4,3/2))
var majorChord = channel(_ => C(1,5/4,3/2))
var offHarmonics = channel(_ => H(1,1.2,1.5,3,7))

function N(scale = 1, seconds = 1) {
  var o = O(440 * scale)
  return {
    seconds
    ,start(t = ctx.currentTime) {
      o.start(t)
      o.stop(t + seconds) }
    ,stop(t = ctx.currentTime) { o.stop(t) }}}

var playC = channel(_ => N(1))

function ARP(/* notes */) {
  var notes = Array.from(arguments)
  var sounds = notes.map
  return {
    start() {
      var t = ctx.currentTime
      notes.forEach((n, i) => {
        notes[i].start(t)
        t += notes[i].seconds || 1
        notes[i].stop(t) })}
    ,stop() {
      notes.forEach(n => n.stop())}}}

var playArp = channel(_ => ARP(N(1), N(5/4), N(3/2)))

var playScale = channel(_ => ARP(N(1), N(9/8), N(5/4), N(4/3), N(3/2), N(5/3), N(15/8), N(2)))

function J(/* fundamentals */) {
  var fundamentals = Array.from(arguments)
  return (freq) => {
    var waves = fundamentals.map(f => O(440 * freq * f, 1 / f))
    return {
      start(t = ctx.currentTime) {waves.forEach(w => w.start(t))}
      ,stop(t = ctx.currentTime) {waves.forEach(w => w.stop(t))}}}}


var playSquareArp = channel(_ => {
  var s = J(1,3,5,7,9,11,13)
  return ARP(s(1), s(5/4), s(3/2)) })

var playSawArp = channel(_ => {
  var s = J(1,2,3,4,5,6,7,8,9,10)
  return ARP(s(1), s(5/4), s(3/2)) })

var monotonousScale = channel(_ => ARP(N(1),
                                       N(1 + 1/12),
                                       N(1 + 2/12),
                                       N(1 + 3/12),
                                       N(1 + 4/12),
                                       N(1 + 5/12),
                                       N(1 + 6/12),
                                       N(1 + 7/12),
                                       N(1 + 8/12),
                                       N(1 + 9/12),
                                       N(1 + 10/12),
                                       N(1 + 11/12),
                                       N(1 + 12/12)))

function S(seconds) {
  return {seconds, start(){}, stop(){}}}

var theme = channel(_ => ARP(N(1 + 8/12, 0.5), N(1 + 7/12, 0.25), N(1 + 6/12, 0.25),
                             S(0.25),
                             N(1 + 7/12, 0.25), N(1 + 8/12, 2)))
