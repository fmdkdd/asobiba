document.addEventListener('DOMContentLoaded', start)

let width = 800
let height = 600
let renderer
let stage
let fullscreen = true
let usePIXI = false

function start() {
  let cb = setup

  if (usePIXI) {
    PIXIRenderer.new(width, height, cb)
  } else {
    CanvasRenderer.new(width, height, cb)
  }
}

function setup(r) {
  renderer = r
  document.body.appendChild(renderer.view)

  document.addEventListener('click', function onMouseClick(ev) {
    // Fullscreen can only be called inside an event handler
    if (!fullscreen) {
      tryFullscreen()
      return
    }

    if (ev.button === 0) {
      let target = pick({x: ev.clientX, y: ev.clientY}, nodes)
      if (target) {
        selectNode(target)
      }
    }
  })

  document.addEventListener('touchstart', function onTouchStart(ev) {
    // Fullscreen can only be called inside an event handler
    if (!fullscreen) {
      tryFullscreen()
      return
    }

    // One-finger tap
    if (ev.touches.length === 1) {
      let target = pick({x: ev.touches[0].clientX, y: ev.touches[0].clientY}, nodes)
      if (target) {
        selectNode(target)
      }
    }

    // Don't raise click events
    ev.preventDefault()
  })

  createGraph()

  requestAnimationFrame(loop)
}

function tryFullscreen() {
  // FIXME: prefix
  renderer.view.mozRequestFullScreen()
  fullscreen = true
}

let lastFrame = 0

function loop(now) {
  let dt = now - lastFrame
  lastFrame = now

  // Update
  transitions.forEach(t => t.update(dt))
  transitions = transitions.filter(t => !t.done)

  edges.forEach(e => { e.updateBounds(); e.color = 0x0000FF })
  checkOverlaps()

  // Draw
  renderer.clear(0xFFFFFF)

  edges.forEach(e => e.draw(renderer))
  nodes.forEach(n => n.draw(renderer))

  renderer.render()

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
      selected: false,
    }
  },

  draw(g) {
    g.fillCircle(this.selected ? 0xAAFF00 : 0x888888, this.x, this.y, 25)
  },

  contains(xy) {
    let dx = xy.x - this.x
    let dy = xy.y - this.y
    let d = Math.sqrt(dx * dx + dy * dy)
    return d < 50
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
      color: 0x888888,
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

  draw(g) {
    g.fillLine(this.color, 4,
               this.poly[0].x, this.poly[0].y,
               this.poly[1].x, this.poly[1].y)
  },
}

function checkOverlaps() {
  for (var i=0, l=edges.length; i < l; ++i ) {
    for (var j=i+1; j < l; ++j) {
      if (do_polygons_collide(edges[i].poly, edges[j].poly)) {
        edges[i].color = edges[j].color = 0xFF0000
      }
    }
  }
}

let nodes = []
let edges = []

function createGraph() {
  nodes.push(node.new(100, 100),
             node.new(100, 300),
             node.new(300, 100),
             node.new(300, 300))

  edges.push(edge.new(nodes[0], nodes[1]),
             edge.new(nodes[0], nodes[2]),
             edge.new(nodes[0], nodes[3]),
             edge.new(nodes[1], nodes[2]))
}


// Renderers

let PIXIRenderer = {
  new(width, height, callback, options) {
    options = options || {}

    let script = document.createElement('script')
    script.src = "pixi.js"
    script.onload = function PIXILoaded() {
      let r = new PIXI.autoDetectRenderer(width, height, {antialias: true})

      // TODO:
      if (options.fillWindow) {
        console.warn('PIXIRenderer fillWindow not implemented')
      }

      callback({
        __proto__: PIXIRenderer,
        _renderer: r,
        _stage: new PIXI.Container(),
      })
    }

    document.body.appendChild(script)
  },

  get view() { return this._renderer.view },

  clear(color) {
    this._stage.removeChildren()
    this._renderer.backgroundColor = color
  },

  render() {
    this._renderer.render(this._stage)
  },

  fillCircle(color, x, y, radius) {
    let g = new PIXI.Graphics()
    g.beginFill(color)
    g.drawCircle(x, y, radius)
    g.endFill()
    this._stage.addChild(g)
  },

  fillLine(color, width, x1, y1, x2, y2) {
    let g = new PIXI.Graphics()
    g.lineStyle(width, color, 1)
    g.moveTo(x1, y1)
    g.lineTo(x2, y2)
    this._stage.addChild(g)
  },
}

let CanvasRenderer = {
  new(width, height, callback, options) {
    options = options || {}

    let canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    let ctxt = canvas.getContext('2d')

    // Use the full available window
    if (options.fillWindow) {
      window.addEventListener('resize', resizeCanvas)
      resizeCanvas()
    }

    function resizeCanvas() {
      canvas.width = window.innerWidth
      canvas.height = window.innerHeight
    }

    callback({
      __proto__: CanvasRenderer,
      _canvas: canvas,
      _ctxt: ctxt,
    })
  },

  get view() { return this._canvas },

  clear(color) {
    this._ctxt.fillStyle = hexToCSS(color)
    this._ctxt.fillRect(0, 0, this._canvas.width, this._canvas.height)
  },

  render() {
  },

  fillCircle(color, x, y, radius) {
    this._ctxt.fillStyle = hexToCSS(color)
    this._ctxt.beginPath()
    this._ctxt.arc(x, y, radius, 0, Math.PI*2)
    this._ctxt.fill()
  },

  fillLine(color, width, x1, y1, x2, y2) {
    this._ctxt.strokeStyle = hexToCSS(color)
    this._ctxt.lineWidth = width
    this._ctxt.beginPath()
    this._ctxt.moveTo(x1, y1)
    this._ctxt.lineTo(x2, y2)
    this._ctxt.stroke()
  },
}

function hexToCSS(color) {
  return `#${color.toString(16).padStart(6, 0)}`
}


// Utils

function first(array, pred) {
  for (let i=0, l=array.length; i < l; ++i) {
    if (pred(array[i]))
      return array[i]
  }
  return null
}
