document.addEventListener('DOMContentLoaded', init)

var bounds
var objects = []

Array.from = [].slice.call.bind([].slice)

function init() {
  bounds = {height: innerHeight, width: innerWidth}

  var p = document.querySelector('p')

  explode(p)

  ;[].forEach.call(p.childNodes, explode)

  objects = Array.from(document.querySelectorAll('.word'))
    .concat(Array.from(document.querySelectorAll('.letter')))

  frame()
}

function frame() {
  objects.forEach(move)

  requestAnimationFrame(frame)
}

function move(element) {
  if (!element.direction) {
    element.rect = element.getBoundingClientRect()
    element.size = Math.max(element.rect.width, element.rect.height)
    element.x = 0
    element.y = 0
    element.angle = 0
    element.direction = { x: Math.random() - .5, y: Math.random() - .5 }
    element.rotation = (Math.random() - .5)  / 500
  }

  element.x += element.direction.x
  element.y += element.direction.y
  element.angle += element.rotation

  var left = element.rect.left + element.x
  var right = element.rect.left + element.x + element.size
  var top = element.rect.top + element.y
  var bottom = element.rect.top + element.y + element.size

  if (left < 2)
    element.direction.x = Math.abs(element.direction.x)
  if (right > bounds.width - 2)
    element.direction.x = -Math.abs(element.direction.x)
  if (top < 2)
    element.direction.y = Math.abs(element.direction.y)
  if (bottom > bounds.height - 2)
    element.direction.y = -Math.abs(element.direction.y)

  element.style.transform = `translate(${element.x}px, ${element.y}px)
                             rotate(${element.angle}rad)`
}

function explode(element) {
  if (element.tagName === 'P')
    explodeIntoWords(element)
  else if (element.tagName === 'SPAN')
    explodeIntoLetters(element)
}

function explodeIntoWords(paragraph) {
  var text = paragraph.textContent
  paragraph.textContent = ''

  text
    .split(' ')
    .map(function(w) { return span(w + ' ', 'word') })
    .forEach(function(w) { paragraph.appendChild(w) })
}

function explodeIntoLetters(word) {
  var text = word.textContent
  word.textContent = ''

  ;[].map.call(text, function(l) { return span(l, 'letter') })
    .forEach(function(l) { word.appendChild(l) })
}

function span(text, type) {
  var e = document.createElement('span')
  e.classList.add(type)
  e.textContent = text
  return e
}
