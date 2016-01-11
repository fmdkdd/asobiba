document.addEventListener("DOMContentLoaded", init)

// Feature 1: draggable boxes
// TODO: Feature 2: link boxes with arcs

function init() {
  d3.select('#add-box')
    .on('click', spawn_box)

  spawn_box()
}



// Heap model

// Heap is an array of JS objects (boxes).
var heap = []

// A box represents a JS object in the visualization.
var box = {
  parent: null,                 // prototype link
  properties: [                 // list of properties
    {label: 'a', value: 1},
    {label: 'b', value: "a"},
  ],

  x: 10, y: 10,                 // top left of the box in the visualization
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

  boxes.each(function(d) {
    var b = d3.select(this)
    d.properties.forEach(function(p, i) {
      var g = b.append('g')
            .attr({transform: `translate(0 ${40 * i})`,})

      // Border of the property
      g.append('rect')
        .attr({
          width: 100,
          height: 40,
          stroke: '#556270',
          fill: 'white',
          'stroke-width': 2,
        })

      // Property name
      g.append('text')
        .attr({
          dx: 5, dy: 25,
          fill: '#556270',
        })
        .text(p.label)

      // Property value
      g.append('text')
        .attr({
          dx: 82, dy: 25,
          'text-anchor': 'middle',
          fill: '#556270',
        })
        .text(p.value)

      // Separating line
      g.append('line')
        .attr({
          x1: 70, y1: 0,
          x2: 70, y2: 40,
          stroke: '#556270',
          'stroke-width': 2,
        })
    })
  })

  // var link_source = null

  // var circle = boxes.append('circle')
  //   .attr({
  //     cx: 80,
  //     cy: 20,
  //     r: 7,
  //   })
  //   .on('mouseover', function circle_mouseover(d) {
  //     d3.select(this).classed('hovered', true)
  //     update_view()
  //   })
  //   .on('mouseout', function circle_mouseexit() {
  //     d3.select(this).classed('hovered', false)
  //     update_view()
  //   })
  //   .on('click', function circle_click(d) {
  //     if (link_source == null) {  // entering linking mode
  //       link_source = d
  //       d3.select(this).classed('selected', true)
  //     }
  //     else { // selecting destination
  //       var link_target = d
  //       link_source.value = link_target
  //       d3.classed('selected', false)
  //     }
  //   })

  // boxes.attr('transform', function(d) { return `translate(${d.x} ${d.y})` })

  // circle.classed('hovered')
  //   .attr('fill', 'red')

  // circle.classed('selected')
  //   .attr('fill', 'blue')
}

var drag_box = d3.behavior.drag()
      .origin(identity)
      .on('drag', function drag_box_on_drag(d) {
        d3.select(this)
          .attr('transform', `translate(${d.x = d3.event.x} ${d.y = d3.event.y})`)
      })
      .on('dragstart', function() {
        // Re-insert as the last child to be drawn above the other boxes
        this.parentNode.appendChild(this);

        d3.select(this).classed('dragged', true)
      })
      .on('dragend', function() {
        d3.select(this).classed('dragged', false)
      })



function identity(d) { return d }
