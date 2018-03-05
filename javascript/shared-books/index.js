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
  shelf_canvas.ctxt.fillStyle = '#ffffff'
  shelf_canvas.ctxt.strokeStyle = '#8888bb'

  quality_canvas.canvas = document.getElementById('quality')
  quality_canvas.canvas.width = 200
  quality_canvas.canvas.height = 200
  quality_canvas.ctxt = quality_canvas.canvas.getContext('2d', {alpha: false})
  quality_canvas.ctxt.lineWidth = 2
  quality_canvas.ctxt.fillStyle = '#ffffff'
  quality_canvas.ctxt.strokeStyle = '#88bb88'
}

function loop() {
  sim()
  refresh()

  requestAnimationFrame(loop)
}

function sim() {
  // Take one random book from a random shelf
  let s = Math.floor(Math.random() * (shelves.length - 2)) + 1
  let b = Math.floor(Math.random() * (shelves[s].length - 1))
  shelves[0].push(shelves[s][b])
  shelves[s].splice(b, 1)

  // Push one random book to a random shelf
  s = Math.floor(Math.random() * (shelves.length - 2)) + 1
  b = Math.floor(Math.random() * (shelves[0].length - 1))
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
  const mid = canvas.height / 2
  for (let x=0; x < shelves.length; ++x) {
    ctxt.beginPath()
    ctxt.moveTo(x*2+1, mid)
    ctxt.lineTo(x*2+1, shelves[x].length - n_books_per_shelf + mid)
    ctxt.stroke()
  }

  // Total quality on shared shelf
  const total_quality = shelves[0].reduce((a,v) => a + qualities[v], 0)
  document.getElementById('total-quality').innerHTML = total_quality

  // Quality over time
  quality_ring[quality_idx] = total_quality
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
}
