document.addEventListener('DOMContentLoaded', start)

let canvas
let ctxt

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

  document.addEventListener('click', function onMouseClick(ev) {
    if (ev.button === 0) {
      let target = pick({x: ev.clientX, y: ev.clientY}, nodes)
      if (target) {
        selectNode(target)
      }
    }
  })

  loop()
}

function loop() {
  // Update
  transitions.forEach(t => t.update(1000 / 60))
  transitions = transitions.filter(t => !t.done)

  edges.forEach(e => {e.updateBounds(); e.color = 'blue'})
  checkOverlaps()

  // Draw
  ctxt.clearRect(0, 0, canvas.width, canvas.height)

  edges.forEach(e => e.draw(ctxt))
  nodes.forEach(n => n.draw(ctxt))

  requestAnimationFrame(loop)
}

let action = {selectedNode: null}

function selectNode(node) {
  // No previously selected node
  if (action.selectedNode == null) {
    action.selectedNode = node
    node.selected = true
  }

  // Otherwise, did we select the same node?
  else if (action.selectedNode === node) {
    // If so, toggle it off
    action.selectedNode = null
    node.selected = false
  }

  // If it's another node, swap them and reset selection
  else {
    swapNodes(action.selectedNode, node)
    action.selectedNode.selected = false
    action.selectedNode = false
    action.selectedNode = null
  }
}

function pick(xy, array) {
  return first(array, el => el.contains(xy))
}

function swapNodes(n1, n2) {
  transitionTo(n1, n2, 300)
  transitionTo(n2, n1, 300)
}

let transitions = []

let transition = {
  new(obj, target, time) {
    return {
      __proto__: transition,
      obj, time,
      target: {x: target.x, y: target.y},
      step: {x: (target.x - obj.x) / time,
             y: (target.y - obj.y) / time},
      elapsed: 0,
      done: false,
    }
  },

  update(dt) {
    this.obj.x += this.step.x * dt
    this.obj.y += this.step.y * dt
    this.elapsed += dt

    if (this.elapsed >= this.time) {
      this.obj.x = this.target.x
      this.obj.y = this.target.y
      this.done = true
    }
  },
}

function transitionTo(p1, p2, time) {
  transitions.push(transition.new(p1, p2, time))
}



// Gameplay stuff

let node = {
  new(x, y) {
    return {
      __proto__: node,
      x, y,
      color: '#888',
      selected: false,
    }
  },

  draw(ctxt) {
    ctxt.fillStyle = this.selected ? 'yellow' : this.color
    ctxt.beginPath()
    ctxt.arc(this.x, this.y, 25, 0, Math.PI*2)
    ctxt.fill()
  },

  contains(xy) {
    let dx = xy.x - this.x
    let dy = xy.y - this.y
    let d = Math.sqrt(dx * dx + dy * dy)
    return d < 25
  },
}

let edge = {
  new(n1, n2) {
    let o = {
      __proto__: edge,
      n1, n2,
      nudge: 20,
      poly: [{x: n1.x, y: n1.y},
             {x: n2.x, y: n2.y}],
      color: '#888',
    }

    o.updateBounds()

    return o
  },

  updateBounds() {
    let angle = Math.atan2(this.n2.y - this.n1.y, this.n2.x - this.n1.x)
    this.poly[0].x = this.n1.x + this.nudge * Math.cos(angle)
    this.poly[0].y = this.n1.y + this.nudge * Math.sin(angle)
    this.poly[1].x = this.n2.x - this.nudge * Math.cos(angle)
    this.poly[1].y = this.n2.y - this.nudge * Math.sin(angle)
  },

  draw(ctxt) {
    ctxt.strokeStyle = this.color
    ctxt.lineWidth = 3
    ctxt.beginPath()
    ctxt.moveTo(this.poly[0].x, this.poly[0].y)
    ctxt.lineTo(this.poly[1].x, this.poly[1].y)
    ctxt.stroke()
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


// Utils

function first(array, pred) {
  for (let i=0, l=array.length; i < l; ++i) {
    if (pred(array[i]))
      return array[i]
  }
  return null
}
