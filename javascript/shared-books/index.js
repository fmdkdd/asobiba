document.addEventListener('DOMContentLoaded', run)

// shelves[0] is the shared shelf
const shelves = []
const qualities = []

const n_shelves = 100
const n_books_per_shelf = 100

function run() {
  init()
  loop()
}

let book_id = 0
function fresh_book() {
  qualities.push(Math.random())
  return book_id++
}

let shelf_canvas = {
  canvas: null,
  ctxt: null,
}

let quality_canvas = {
  canvas: null,
  ctxt: null,
}

function init() {
  for (let i=0; i < n_shelves; ++i) {
    const s = []
    shelves.push(s)
    for (let j=0; j < n_books_per_shelf; ++j) {
      s.push(fresh_book())
    }
  }

  shelf_canvas.canvas = document.getElementById('shelves')
  shelf_canvas.canvas.width = n_shelves * 2
  shelf_canvas.canvas.height = n_books_per_shelf * 2
  shelf_canvas.ctxt = shelf_canvas.canvas.getContext('2d', {alpha: false})
  shelf_canvas.ctxt.lineWidth = 2
  shelf_canvas.ctxt.fillStyle = '#fff'
  shelf_canvas.ctxt.strokeStyle = '#88f'

  quality_canvas.canvas = document.getElementById('quality')
  quality_canvas.canvas.width = 200
  quality_canvas.canvas.height = 200
  quality_canvas.ctxt = quality_canvas.canvas.getContext('2d', {alpha: false})
  quality_canvas.ctxt.lineWidth = 2
  quality_canvas.ctxt.fillStyle = '#fff'
  quality_canvas.ctxt.strokeStyle = '#8d8'
}

function loop() {
  sim()
  refresh()

  requestAnimationFrame(loop)
}

function sim() {
  // Take one random book from a random shelf
  let s = rand_weighted(shelves.slice(1).map(s => s.length)) + 1
  // Lower quality books are more likely to be given away
  let b = rand_weighted(shelves[s].map(b => 1 - qualities[b]))
  shelves[0].push(shelves[s][b])
  shelves[s].splice(b, 1)

  // Push one random book to a random shelf
  s = rand_weighted(shelves.slice(1).map(s => -s.length)) + 1
  // Higher quality books are more likely to be taken away
  b = rand_weighted(shelves[0].map(b => qualities[b]))
  shelves[s].push(shelves[0][b])
  shelves[0].splice(b, 1)
}

const quality_ring = new Array(100)
let quality_idx = 0

function refresh() {
  document.getElementById('shared').innerHTML = shelves[0].length

  // Number of books per shelves
  let ctxt = shelf_canvas.ctxt
  let canvas = shelf_canvas.canvas
  ctxt.fillRect(0,0,canvas.width,canvas.height)
  let mid = canvas.height / 2
  for (let x=0; x < shelves.length; ++x) {
    ctxt.beginPath()
    ctxt.moveTo(x*2+1, mid)
    ctxt.lineTo(x*2+1, mid - (shelves[x].length - n_books_per_shelf))
    ctxt.stroke()
  }

  // Total quality on shared shelf
  const q0 = shelves[0].reduce((a,v) => a + qualities[v], 0)
  document.getElementById('total-quality').innerHTML = Math.round(q0)

  // Quality over time
  quality_ring[quality_idx] = q0
  quality_idx = (quality_idx + 1) % quality_ring.length

  ctxt = quality_canvas.ctxt
  canvas = quality_canvas.canvas
  ctxt.fillRect(0,0,canvas.width,canvas.height)
  ctxt.beginPath()
  ctxt.moveTo(0, canvas.height - quality_ring[quality_idx])
  for (let x=1; x < quality_ring.length; ++x) {
    ctxt.lineTo(x*2, canvas.height - Math.floor(quality_ring[(quality_idx + x) % quality_ring.length]))
  }
  ctxt.stroke()

  mid = canvas.height / 2
  for (let x=0; x < shelves.length; ++x) {
    const q = shelves[x].reduce((a,v) => a + qualities[v], 0)
    ctxt.beginPath()
    ctxt.moveTo(x*2+1, mid)
    ctxt.lineTo(x*2+1, mid - (q - q0))
    ctxt.stroke()
  }
}

// Return a random index from an array of weights
function rand_weighted(weights) {
  // Sum all weights
  const sum = weights.reduce((a,v) => a + v)

  // Make an array of probabilities that sum to 1
  const probs = weights.map(w => w / sum)

  // Make a wheel of fortune from the successive probs
  let running_prob = 0
  const wheel = probs.map(p => running_prob += p)

  // Pick a number!
  const n = Math.random()

  // Find the the number in the wheel, and return its index
  return wheel.findIndex(p => n < p)
}
