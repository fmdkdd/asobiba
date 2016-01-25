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

  boxes.each(function construct_box(d) {
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
        .on('click', edit_text)
      // Prevent the drag behavior from interfering with the cursor selection
        .on('mousedown', function() { d3.event.stopPropagation() })

      // Property value
      g.append('text')
        .attr({
          dx: 82, dy: 25,
          'text-anchor': 'middle',
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

}

function edit_text() {
  // Add ability to edit SVG text element by spawning a contenteditable div
  // above the SVG text element.  When the text is saved, change the value of
  // the SVG text element and destroy the temporary div.
  //
  // I tried to set document.body to contenteditable, which enables editing the
  // SVG text element directly, but the caret is mightily broken in Firefox and
  // Chrome.
  //
  // If the contenteditable div proves unreliable across browsers, maybe use an
  // input text instead.

  // Prevent the click from triggering further interaction.
  d3.event.stopPropagation()

  var text_orig = d3.select(this)
  var bb = this.getBoundingClientRect()

  // Place the div just above the text_orig element
  var left = window.scrollX + bb.left
  var top = window.scrollY + bb.top

  var edit_box = d3.select('body').append('div')
        .attr('class', 'tmp-edit-box')
        .attr('contenteditable', true)
        .style({position: 'absolute',
                left: left + 'px',
                top: top + 'px'})
        .text(text_orig.text())

  // Hide original text element
  text_orig.style('visibility', 'hidden')

  // Save cursor position in SVG text element
  var sel = getSelection()
  var cursorPos = getSelection().anchorOffset

  // Focus the div instead
  edit_box.node().focus()
  // and put the cursor at the same place in the text
  sel.collapse(edit_box.node().firstChild, cursorPos)

  // When the edit is done, save the text value, destroy the edit box, and
  // unhide the original SVG text element
  function finish_edit() {
    text_orig.text(edit_box.text())
    edit_box.remove()
    text_orig.style('visibility', 'visible')

    // No need to bubble
    d3.event.stopPropagation()
  }

  // The edit ends when we focus away from the edit box
  edit_box.on('blur', finish_edit)
  // or when we type enter
  edit_box.on('keypress', function() {
    // No need to bubble
    d3.event.stopPropagation()

    if (d3.event.keyCode === 13) { // 'return' keycode
      finish_edit()
    }
  })
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
