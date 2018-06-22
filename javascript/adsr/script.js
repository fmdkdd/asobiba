document.addEventListener('DOMContentLoaded', init)

let ctx
let tl = 50
const max_tl = 200
let ar = 60
const max_ar = 90
let dr = 20
const max_dr = 90
let sl = 25
const max_sl = 200

const height = 256
const width  = 512
const margin = 10
const origin = [margin,height]

function init() {
  let canvas = document.getElementById('canvas')
  ctx = canvas.getContext('2d')

  canvas.width  = width  + margin
  canvas.height = height + margin

  // Setup listeners
  document.getElementById('tl').addEventListener('change', function() { tl = this.value })
  document.getElementById('ar').addEventListener('change', function() { ar = this.value })
  document.getElementById('sl').addEventListener('change', function() { sl = this.value })
  document.getElementById('dr').addEventListener('change', function() { dr = this.value })

  loop()
}

function loop(t) {
  requestAnimationFrame(loop)

  ctx.clearRect(0,0,width+margin,height+margin)

  // Draw axes
  ctx.strokeStyle = 'black'
  ctx.beginPath()
  ctx.moveTo(margin,0)
  ctx.lineTo(margin,height)
  ctx.lineTo(width+margin,height)
  ctx.stroke()

  // Draw TL
  ctx.strokeStyle = 'blue'
  ctx.beginPath()
  ctx.moveTo(margin,origin[1]-tl)
  ctx.lineTo(width+margin,origin[1]-tl)
  ctx.stroke()

  // Draw SL
  ctx.strokeStyle = 'blue'
  ctx.beginPath()
  ctx.moveTo(margin,origin[1]-sl)
  ctx.lineTo(width+margin,origin[1]-sl)
  ctx.stroke()

  // Draw attack
  let x = origin[0]
  let y = origin[1]
  ctx.strokeStyle = 'red'
  ctx.beginPath()
  ctx.moveTo(x,y)
  const ar_angle = ar * Math.PI/2  / max_ar
  const attack_length = tl / Math.tan(ar_angle)
  x += attack_length
  y -= tl
  ctx.lineTo(x,y)

  // Draw decay
  const dr_angle = dr * Math.PI/2  / max_dr
  const decay_length = (tl-sl) / Math.tan(dr_angle)
  x += decay_length
  y += (tl-sl)
  ctx.lineTo(x,y)

  ctx.stroke()
}
