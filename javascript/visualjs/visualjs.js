document.addEventListener("DOMContentLoaded", init)

// Feature 1: draggable boxes
// TODO: Feature 2: link boxes with arcs

function init() {
  d3.select('#add-box')
    .on('click', spawn_box)

  spawn_box()
}



// Heap model

var heap = []

var box = {
  parent: null,
  label: 'a',
  value: 1,
  next: null,  // next property, if any

  x: 10, y: 10,
}


// Heap view

function spawn_box() {
  var b = Object.create(box)
  heap.push(b)
  update_view()
}

function update_view() {
  var svg = d3.select('#box-area')

  var boxes_join = svg.selectAll('.box')
        .data(heap)

  // boxes.exit().remove()

  var boxes = boxes_join.enter().append('g')
        .attr('class', 'box')
        .call(drag_box)

  boxes.append('rect')
    .attr({
      width: 100,
      height: 40,
      stroke: 'black',
      fill: 'white',
      'stroke-width': 2,
    })

  boxes.append('circle')
    .attr({
      cx: 80,
      cy: 20,
      r: 7,
    })

  boxes.attr('transform', function(d) { return `translate(${d.x} ${d.y})` })
}

var drag_box = d3.behavior.drag()
      .origin(identity)
      .on('drag', function drag_box_on_drag(d) {
        d3.select(this)
          .attr('transform', `translate(${d.x = d3.event.x} ${d.y = d3.event.y})`)
      })
      .on('dragstart', function() {
        d3.select(this).classed('dragged', true)
      })
      .on('dragend', function() {
        d3.select(this).classed('dragged', false)
      })



function identity(d) { return d }
