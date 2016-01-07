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
  var b = box(10, 10, 100, 40)

  document.querySelector('#box-area')
    .appendChild(b)
}

function box(x, y, width, height) {
  var $box = fromTemplate('#template-box').querySelector('.box')
  $box.setAttribute('width', width)
  $box.setAttribute('height', height)
  $box.setAttribute('x', x)
  $box.setAttribute('y', y)
  return $box
}



function handle_draggables($area) {
  var mouse_start = {x: 0, y: 0}
  var draggable = null

  $area.addEventListener('mousedown', function(e) {
    if (e.buttons === 1 && is_draggable(e.target)) {
      mouse_start.x = e.clientX
      mouse_start.y = e.clientY
      draggable = e.target
      draggable.classList.add('dragged')
      e.preventDefault()
    }
  })

  $area.addEventListener('mousemove', function(e) {
    if (draggable) {
      var dx = e.clientX - mouse_start.x
      var dy = e.clientY - mouse_start.y
      draggable.setAttribute('transform', `translate(${dx} ${dy})`)
      e.preventDefault()
    }
  })

  $area.addEventListener('mouseup', function(e) {
    if (draggable) {
      var dx = e.clientX - mouse_start.x
      var dy = e.clientY - mouse_start.y
      var x = parseInt(draggable.getAttribute('x')) + dx
      var y = parseInt(draggable.getAttribute('y')) + dy
      draggable.setAttribute('x', x)
      draggable.setAttribute('y', y)
      draggable.removeAttribute('transform')
      draggable.classList.remove('dragged')
      draggable = null
      e.preventDefault()
    }
  })
}

function is_draggable($elem) {
  return $elem.hasAttribute('data-draggable')
}



function fromTemplate(selector) {
  var $template = document.querySelector(selector)
  var $fragment = document.importNode($template.content, true)
  return $fragment
}

function dispatch(eventName, detail) {
  document.dispatchEvent(new CustomEvent(eventName, {detail}))
}
