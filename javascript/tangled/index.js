document.addEventListener('DOMContentLoaded', start)

let canvas
let ctxt
let mouse = {x: 0, y: 0}

function start() {
  canvas = document.getElementById("canvas")
  ctxt = canvas.getContext('2d')

  // Use the full window please
  window.addEventListener('resize', resizeCanvas)
  function resizeCanvas() {
    canvas.width = window.innerWidth
    canvas.height = window.innerHeight
  }
  resizeCanvas()

  document.addEventListener('mousemove', updateMouse)

  function updateMouse(ev) {
    mouse.x = ev.clientX
    mouse.y = ev.clientY
  }

  draw()
}

function draw() {
  ctxt.clearRect(0, 0, canvas.width, canvas.height)

  edges[0].poly[0].x = mouse.x
  edges[0].poly[0].y = mouse.y + 5
  edges[0].poly[1].x = mouse.x
  edges[0].poly[1].y = mouse.y

  edges.forEach(e => e.color = 'blue')
  checkOverlaps()

  // nodes.forEach(n => n.draw(ctxt))
  edges.forEach(e => e.draw(ctxt))

  requestAnimationFrame(draw)
}



// Gameplay stuff

let node = {
  new(x, y) {
    return {
      __proto__: node,
      x, y,
      color: 'blue',
    }
  },

  draw(ctxt) {
    ctxt.fillStyle = this.color
    ctxt.beginPath()
    ctxt.arc(this.x, this.y, 25, 0, Math.PI*2)
    ctxt.fill()
  },
}

let edge = {
  new(n1, n2) {
    return {
      __proto__: edge,
      n1, n2,
      poly: [{x: n1.x - 5, y: n1.y},
             {x: n1.x + 5, y: n1.y - 5},
             {x: n2.x + 5, y: n2.y},
             {x: n2.x - 5, y: n2.y + 5}],
      color: 'blue',
    }
  },

  draw(ctxt) {
    ctxt.fillStyle = this.color
    ctxt.lineWidth = 1
    ctxt.beginPath()
    ctxt.moveTo(this.poly[0].x, this.poly[1].y)
    for (var i=1, l=this.poly.length; i < l; ++i) {
      ctxt.lineTo(this.poly[i].x, this.poly[i].y)
    }
    ctxt.closePath()
    ctxt.fill()
  },
}

let nodes = []
let edges = []

nodes.push(node.new(100, 100),
           node.new(100, 300),
           node.new(300, 100),
           node.new(300, 300))

edges.push(edge.new(nodes[0], nodes[1]),
           edge.new(nodes[0], nodes[2]),
           edge.new(nodes[0], nodes[3]),
           edge.new(nodes[1], nodes[2]))

function checkOverlaps() {
  for (var i=0, l=edges.length; i < l; ++i ) {
    for (var j=i+1; j < l; ++j) {
      if (do_polygons_collide(edges[i].poly, edges[j].poly)) {
        edges[i].color = edges[j].color = 'red'
      }
    }
  }
}
