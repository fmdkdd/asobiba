document.addEventListener("DOMContentLoaded", init)

// Feature 1: draggable boxes
// TODO: Feature 2: link boxes with arcs

function init() {
  document.querySelector('#add-box')
    .addEventListener('click', add_box)

  var $box_area = document.querySelector('#box-area')
  handle_draggables($box_area)
}

function add_box() {
  var b = box(10, 10)

  document.querySelector('#box-area')
    .appendChild(b)
}

function box(x, y) {
  var $box = fromTemplate('#template-box').querySelector('.box')
  $box.setAttribute('transform', `translate(${x} ${y})`)
  return $box
}

function box_x(b) {return b.transform.baseVal[0].matrix.e}
function box_y(b) {return b.transform.baseVal[0].matrix.f}
function box_x_set(b, x) {b.transform.baseVal[0].matrix.e = x}
function box_y_set(b, y) {b.transform.baseVal[0].matrix.f = y}

function path_between_boxes(b1, b2) {
  var $arc = fromTemplate('#template-arc').querySelector('.arc')
  $arc.setAttribute('d', 'M 0 0 L 10 10')
  return $arc
}



function handle_draggables($area) {
  var mouse_start = {x: 0, y: 0}
  var elem_start = {x: 0, y: 0}
  var draggable = null

  $area.addEventListener('mousedown', function(e) {
    var d
    if (e.buttons === 1 && (d = find_draggable(e.target))) {
      mouse_start.x = e.clientX
      mouse_start.y = e.clientY
      draggable = d
      elem_start.x = box_x(draggable)
      elem_start.y = box_y(draggable)
      draggable.classList.add('dragged')
      e.preventDefault()
    }
  })

  $area.addEventListener('mousemove', function(e) {
    if (draggable) {
      var dx = e.clientX - mouse_start.x
      var dy = e.clientY - mouse_start.y
      box_x_set(draggable, elem_start.x + dx)
      box_y_set(draggable, elem_start.y + dy)
      e.preventDefault()
    }
  })

  $area.addEventListener('mouseup', function(e) {
    if (draggable) {
      draggable.classList.remove('dragged')
      draggable = null
      e.preventDefault()
    }
  })
}

function is_draggable($elem) {
  return $elem.hasAttribute('data-draggable')
}

function find_draggable($elem) {
  var e = $elem
  while (e && !is_draggable(e)) {
    e = e.parentElement
  }
  return e
}



function fromTemplate(selector) {
  var $template = document.querySelector(selector)
  var $fragment = document.importNode($template.content, true)
  return $fragment
}

function dispatch(eventName, detail) {
  document.dispatchEvent(new CustomEvent(eventName, {detail}))
}
