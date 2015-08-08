document.addEventListener('DOMContentLoaded', init)

var width = 800
var height = 600

var canvas
var ctxt

var allBoxes = []
var grid
var mouse = {x:0, y:0}

function spawn_box() {
  var b =  box(point(Math.random() * width, Math.random() * height),
               30, 30)

  grid.insertObjectWithBoundingBox(b, b)
  allBoxes.push(b)
}

function init() {
  canvas = document.querySelector('canvas')
  ctxt = canvas.getContext('2d')

  canvas.width = width
  canvas.height = height

  canvas.addEventListener('mousemove', function(e) {
    mouse.x = e.clientX
    mouse.y = e.clientY
  })

  grid = spatialHash.new(60)

  for (var i = 0; i < 1000; ++i)
    spawn_box()

  for (var b of allBoxes) {
    ctxt.strokeStyle = '#666'
    ctxt.strokeRect(b.x, b.y, b.width, b.height)
  }

 frame()
}

function frame() {
  var nearBoxes = grid.objectsNearPoint(mouse)
  var mouseBox = box(mouse, 1, 1)

  ctxt.fillStyle = '#a66'

  for (var b of nearBoxes) {
    if (do_boxes_collide(mouseBox, b)) {
      ctxt.fillRect(b.x, b.y, b.width, b.height)
    }
  }

  requestAnimationFrame(frame)
}
