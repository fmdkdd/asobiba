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
  $box.style.width = `${width}px`
  $box.style.height = `${height}px`
  $box.style.left = `${x}px`
  $box.style.top = `${y}px`
  $box.setAttribute('data-x', x)
  $box.setAttribute('data-y', y)
  return $box
}



function handle_draggables($area) {
  var drag_start = {x: 0, y: 0}
  var draggable = null
  var dx = 0
  var dy = 0

  $area.addEventListener('mousedown', function(e) {
    if (e.buttons === 1 && is_draggable(e.target)) {
      drag_start.x = e.clientX
      drag_start.y = e.clientY
      dx = 0
      dy = 0
      draggable = e.target
      draggable.classList.add('dragged')
    }
  })

  $area.addEventListener('mousemove', function(e) {
    if (draggable) {
      dx = e.clientX - drag_start.x
      dy = e.clientY - drag_start.y
      draggable.style.transform = `translate(${dx}px, ${dy}px)`
    }
  })

  $area.addEventListener('mouseup', function(e) {
    if (draggable) {
      var x = parseInt(draggable.getAttribute('data-x')) + dx
      var y = parseInt(draggable.getAttribute('data-y')) + dy
      draggable.setAttribute('data-x', x)
      draggable.setAttribute('data-y', y)
      draggable.style.transform = ''
      draggable.style.left = `${x}px`
      draggable.style.top = `${y}px`
      draggable.classList.remove('dragged')
      draggable = null
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
