document.addEventListener('DOMContentLoaded', init)

// Model
var boxes = [{x: 23, y: 42}, {x: 80, y: 260}]
var links = new Map()

var $svg = null

function init() {
  // Setup all interesting listeners on root.
  $svg = document.querySelector('#box-area')

  $svg.addEventListener('click', update)
  document.addEventListener('mousemove', update)
  $svg.addEventListener('mouseenter', update)

  boxes.forEach(b => {
    var box = Box.new(b)
    box.moveTo(b.x, b.y)
    box.add(Property.new().$)
    box.insertInto($svg)
    // $svg.appendChild(createBox(b))
  })
}

var Property = {
  name: 'name',
  value: 0,
  $: null,

  new(name, value) {
    var o = Object.create(this)

    o.name = name
    o.value = value

    o.$ = svgElem('g.property')
    var rect = svgElem('rect', {width: 200, height: 80})
    o.$.appendChild(rect)
    var circle = svgElem('circle', {cx: 160, cy: 40, r: 15})
    o.$.appendChild(circle)

    return o
  },

  insertInto($elem) {
    $elem.appendChild(this.$)
  },
}

var Box = {
  x: 0, y: 0,
  $: null,
  dragStart: {x: 0, y: 0},

  new() {
    var o = Object.create(this)

    o.$ = svgElem('g.box')

    o.$.addEventListener('mousedown', ev => {
      o.$.classList.add('dragging')
      o.dragStart.x = ev.clientX
      o.dragStart.y = ev.clientY
      ev.preventDefault()
    })

    document.addEventListener('mouseup', _ => {
      o.$.classList.remove('dragging')
    })

    document.addEventListener('mousemove', ev => {
      if (o.$.classList.contains('dragging')) {
        o.moveBy(ev.clientX - o.dragStart.x,
                 ev.clientY - o.dragStart.y)
        o.dragStart.x = ev.clientX
        o.dragStart.y = ev.clientY
      }
    })

    return o
  },

  moveTo(x, y) {
    this.x = x; this.y = y
    this.$.setAttribute('transform', `translate(${x} ${y})`)
  },

  moveBy(dx, dy) {
    this.moveTo(this.x + dx, this.y + dy)
  },

  insertInto($elem) {
    $elem.appendChild(this.$)
  },

  add($elem) {
    this.$.appendChild($elem)
  },

  empty() {
    while (this.$.firstChild) {
      this.$.removeChild(this.$.firstChild)
    }
  },
}

function update(event) {
  var el

  if ((el = matches('circle:click', event)) && !$svg.querySelector('.tmp-link')) {
    var $from = el
    var from_bb = relativeBBox($from, $svg)

    // Add link on top
    var link = svgElem('line.tmp-link', {
      x1: from_bb.cx, y1: from_bb.cy,
      x2: from_bb.cx, y2: from_bb.cy,
    })
    $svg.insertBefore(link, $svg.firstChild)
  }

  else if ((el = matches('.box:mousemove', event)) && $svg.querySelector('.tmp-link')) {
    var l = $svg.querySelector('.tmp-link')
    var bb = relativeBBox(el, $svg)
    l.setAttribute('x2', bb.x)
    l.setAttribute('y2', bb.y)
  }

  else if (matches(':mousemove', event) && $svg.querySelector('.tmp-link')) {
    var l = $svg.querySelector('.tmp-link')
    l.setAttribute('x2', event.clientX)
    l.setAttribute('y2', event.clientY)
  }

  else if (matches('.box:click', event) && $svg.querySelector('.tmp-link')) {
    $svg.querySelector('.tmp-link')
      .setAttribute('class', 'link')
  }
}

function matches(str, event) {
  var a = str.split(':')
  var css = a[0]
  var type = a[1]

  if (event.type !== type) return false

  // Empty CSS selector always matches
  if (!css) return true

  // Walk up the tree to find a matching element
  var t = event.target
  while (t) {
    if (t.matches(css)) return t
    t = t.parentElement
  }

  return false
}

function svgElem(type, attrs) {
  var a = type.split('.')
  type = a.shift()
  var classes = a.join(' ')

  var el = document.createElementNS('http://www.w3.org/2000/svg', type)
  if (classes)
    el.setAttribute('class', classes)
  for (var a in attrs) {
    el.setAttribute(a, attrs[a])
  }
  return el
}

function createBox(box) {
  var g = svgElem('g.box', {transform: `translate(${box.x} ${box.y})`})
  g.appendChild(
    svgElem('rect', {width: 200, height: 80}))
  g.appendChild(
    svgElem('circle', {cx: 160, cy: 40, r: 15}))
  return g
}

// Return bounding box of elem relative to the top left corner of the given
// container.  The bounding box is an object with the properties: top (alias y),
// left (alias x), down, right, width, height, cx (x of center), cy (y of
// center)
function relativeBBox(elem, container) {
  var bb = elem.getBoundingClientRect()
  var ref = container.getBoundingClientRect()

  var left = bb.left - ref.left
  var top = bb.top - ref.top

  return {
    left: left, x: left,
    top: top, y: top,
    right: left + bb.width,
    down: top + bb.height,
    width: bb.width,
    height: bb.height,
    cx: left + bb.width / 2,
    cy: top + bb.height / 2
  }
}
