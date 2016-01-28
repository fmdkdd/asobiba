document.addEventListener("DOMContentLoaded", init)

// Feature 1: draggable boxes
// Feature 2: link boxes with arcs
// TODO: update links when boxes move
// TODO: can add/remove properties

function init() {
  d3.select('#add-box')
    .on('click', spawn_box)

  spawn_box()
}



// Heap model

var ref = {}

// Heap is an array of JS objects (boxes).
var heap = []

// A box represents a JS object in the visualization.
var box = {
  id: -1,
  parent: null,                 // prototype link
  properties: [                 // list of properties
    {label: 'a', value: 1},
    {label: 'b', value: ref},
  ],

  x: 10, y: 10,                 // top left of the box in the visualization
}


// Heap view

var gen_id = 0
function spawn_box() {
  var b = Object.create(box)
  b.id = gen_id++
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

  boxes.each(function construct_box(d) {
    var b = d3.select(this)
    d.properties.forEach(function(p, i) {
      var g = b.append('g')
            .attr({
              class: 'cell',
              transform: `translate(0 ${40 * i})`,})

      // Border of the property
      g.append('rect')
        .attr({
          width: 100,
          height: 40,
          stroke: '#556270',
          fill: 'transparent',
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
      if (p.value === ref) {
        g.append('circle')
          .attr({
            class: 'ref',
            cx: 85, cy: 20, r: 4
          })
        // Prevent the drag behavior from interfering with the ref click
          .on('mousedown', function() { d3.event.stopPropagation() })
      } else {
        g.append('text')
          .attr({
            dx: 82, dy: 25,
            'text-anchor': 'middle',
          })
          .text(p.value)
      }

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


  // On click on circle, begin linking by overlaying a temporary path between
  // the link source and the mouse cursor.  When a second target is clicked, add
  // the link to the model, remove the overlay, and add the definitive link
  // path.  If the second click happens outside of a valid target, cancel
  // linking by destroying the overlay.

  var link_automaton = automaton.new(svg.node())
  var ready = state.new(link_automaton)
  var src_selected = state.new(link_automaton)
  var inside_cell = state.new(link_automaton)

  var grow = animate_radius(10)
  var shrink= animate_radius(4)
  var stroke_green = animate_stroke('#4ECDC4')
  var stroke_black = animate_stroke('#556270')

  ready
    .addListener('enter', function() {
      // Reset data linked to the automaton
      link_automaton.data.link_src = null
      link_automaton.data.link_dst = null
      link_automaton.data.current_cell = null

      // Can drag boxes in this state
      boxes.call(drag_box)
    })

    .addListener('leave', function() {
      // Cannot drag in the other states
      boxes.on('.drag', null)
    })

    .to(inside_cell, '.ref:click', function() {
      link_automaton.data.current_cell = this.parentNode
      add_tmp_link.call(this)
    })

    .on('.ref:mouseenter', grow)
    .on('.ref:mouseleave', shrink)

  src_selected
    .on('body:mousemove', update_tmp_link)

    .to(inside_cell, '.cell:mouseenter', function() {
      link_automaton.data.current_cell = this
    })

  // Clicking anywhere else cancels linking
    .to(ready, 'svg:click', function() {
      remove_tmp_link.call(this)

      // When the link is added or canceled, shrink the circle back.  This is needed
      // because we voluntarily leave the first selected circle in the grow state.
      shrink.call(link_automaton.data.link_src)
    })

  inside_cell
    .addListener('enter', select_cell)
    .addListener('leave', function() {
      link_automaton.data.current_cell = null

      svg.selectAll('.tmp-anchor')
        .remove()
    })

    .on('.cell:mousemove', select_cell)

  // Clicking a cell installs the link from the source to it
    .to(ready, '.cell:click', function() {
      add_link.call(this)
      shrink.call(link_automaton.data.link_src)
    })

    .to(src_selected, '.cell:mouseleave', function() {
      stroke_black.call(this.querySelector('rect'))
    })

  // The initial state
  link_automaton.enter(ready)



  function add_tmp_link() {
    var src = link_automaton.data.link_src = this

    // Get coordinates relative to containing SVG
    var src_bb = relativeBBox(src, svg.node())
    // and coordinates of mouse relative to the same container
    var mouse = d3.mouse(svg.node())

    var link = null

    // If the src already has a link pointing from it, then re-use the link
    // rather than create a new one.
    if (src.__data__.svg_link) {
      link = d3.select(src.__data__.svg_link)

      // and remove the link from the model
      // links.delete(src.__data__)
      src.__data__.svg_link = null
    } else {
      // Build line from src to mouse.  Add before other elements to the circles
      // appear on top of the line.
      link = svg.insert('path', ':first-child')
    }

    link .attr({class: 'tmp-link',
                d: link_path({
                  cell: src.parentNode,
                  xy: [src_bb.cx, src_bb.cy]
                }, {mouse: mouse})})
             // x1: src_bb.cx, y1: src_bb.cy,
             // x2: mouse[0], y2: mouse[1]})

    // No bubbling necessary
    d3.event.stopPropagation()
  }

  function update_tmp_link() {
    var src = link_automaton.data.link_src
    var src_bb = relativeBBox(src, svg.node())
    var mouse = d3.mouse(svg.node())
    var link = svg.select('.tmp-link')
          .attr('d', link_path({
            cell: src.parentNode,
            xy: [src_bb.cx, src_bb.cy]
          }, {mouse: mouse}))
    // svg.select('.tmp-link')
    //   .attr({x2: mouse[0], y2: mouse[1]})
  }

  function link_path(from, to) {
    var line = d3.svg.line()

    var src = from.xy
    var dst = null
    var pre_dst = null

    var path = [src]

    if (to.mouse) {
      dst = to.mouse
    } else if (to.cell) {
      // If targeting a cell, there are only three clean entries: up, left,
      // bottom.  Up and bottom can be obscured by neighboring cells.

      // Use the mouse to select the nearest valid anchor.
      var mouse = d3.mouse(svg.node())
      var cell = to.cell

      var anchors = valid_cell_anchors(cell)

      var nearest = _.minBy(anchors, function(p) {
        return distance(mouse, p)
      })

      dst = [nearest[0], nearest[1]]
      pre_dst = [nearest[2], nearest[3]]
    }

    // There are only two clean exits out of ref circle: right and bottom.  We
    // should exit from the bottom when the destination is in the left part of
    // the diagonal sprouting from the ref circle to the top-left corner of the
    // box.
    //
    //           \
    //            \   Right exit
    //             \
    //              o
    //               \
    // Bottom exit    \
    //                 \
    var box_bb = relativeBBox(from.cell.parentNode, svg.node())

    var side = side_of_AB(src, [box_bb.x, box_bb.y], dst)

    if (side < 0) {             // bottom exit
      path.push([src[0], src[1] + 40])
      line.interpolate('step-after')
    } else {                    // right exit
      path.push([src[0] + 30, src[1]])
      line.interpolate('step-before')
    }

    // If targeting a cell, add a point to the path to avoid meeting the anchor
    // destination with a parallel line.
    if (pre_dst)
      path.push(pre_dst)

    path.push(dst)

    return line(path)
  }

  function select_cell() {
    var src = link_automaton.data.link_src
    var src_bb = relativeBBox(src, svg.node())

    var cell = link_automaton.data.current_cell
    stroke_green.call(cell.querySelector('rect'))

    // Draw all valid anchors to guide the user
    var anchors = svg.selectAll('.tmp-anchor')
      .data(valid_cell_anchors(cell))

    anchors.enter()
      .append('circle')
      .attr('class', 'tmp-anchor')
      .attr('r', 4)
      .attr('fill', '#4ECDC4')

    anchors
      .attr('cx', function(d) { return d[0] })
      .attr('cy', function(d) { return d[1] })

    // Snap tmp link to nearest anchor
    var bb = relativeBBox(cell, svg.node())
    svg.select('.tmp-link')
      .attr('d', link_path({
        cell: cell,
        xy: [src_bb.cx, src_bb.cy]
      }, {cell: cell}))

      // .attr({x2: bb.x, y2: bb.y})
  }

  function valid_cell_anchors(cell) {
    var cell_bb = relativeBBox(cell, svg.node())

    // left anchor is alway available
    var anchors = [
      [cell_bb.x     , cell_bb.y + cell_bb.height / 2,
       cell_bb.x - 15, cell_bb.y + cell_bb.height / 2]
    ]

    var box = cell.parentNode
    var box_bb = relativeBBox(box, svg.node())

    // If top cell, up anchor is available
    if (cell_bb.y === box_bb.y)
      anchors.push([cell_bb.x + 35, cell_bb.y,
                    cell_bb.x + 35, cell_bb.y - 15])

    // If bottom cell, bottom anchor is available
    if (cell_bb.y + cell_bb.height === box_bb.y + box_bb.height)
      anchors.push([cell_bb.x + 35, cell_bb.y + cell_bb.height,
                    cell_bb.x + 35, cell_bb.y + cell_bb.height + 15])

    return anchors
  }

  function remove_tmp_link() {
    svg.select('.tmp-link').remove()
  }

  function add_link() {
    var src = link_automaton.data.link_src
    var dst = this

    // Create permanent link between the two in the model
    // links.set(src.__data__, dst.__data__)

    // Promote the temporary link to permanent
    var link = svg.select('.tmp-link')
      .attr('class', 'link')

    // and associate the SVG link with its endpoints
    src.__data__.svg_link = link.node()

    // Also stroke the selected cell back to black immediately
    stroke_black.call(dst.querySelector('rect'))
  }


  // Animations functions
  function animate_radius(r) {
    return function() {
      d3.select(this)
        .transition()
        .duration(300)
        .ease('elastic')
        .attr('r', r)
    }
  }

  function animate_stroke(color) {
    return function() {
      d3.select(this)
        .transition()
        .duration(300)
        .ease('elastic')
        .attr('stroke', color)
    }
  }
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
        // Snap to grid of 10x10 pixels
        var x = 10 * Math.floor(d3.event.x / 10)
        var y = 10 * Math.floor(d3.event.y / 10)

        d3.select(this)
          .attr('transform', `translate(${d.x = x} ${d.y = y})`)

        // TODO: update links pointing to or from this box
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

// A, B and P are three points.  Returns -1 if P is to the left of (AB), +1 if
// on the right, and 0 if P is on the line.
// https://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line
function side_of_AB(A, B, P) {
  return Math.sign((B[0] - A[0]) * (P[1] - A[1])
                   - (B[1] - A[1]) * (P[0] - A[0]))
}

function distance(a, b) {
  var dx = a[0] - b[0]
  var dy = a[1] - b[1]
  return Math.sqrt(dx * dx + dy * dy)
}


// State automaton

var automaton = {
  new: function(root) {
    var o = Object.create(this)

    o.root = root
    o.states = Object.create(null)
    o.currentState = null
    // Holds state local to the automaton
    o.data = Object.create(null)

    return o
  },

  enter: function(state) {
    // Leave whatever state we were in to trigger the leave event.
    if (this.currentState)
      this.currentState.leave()

    // Enter the new
    this.currentState = state
    this.currentState.enter()
  }
}

var state = {
  new: function(automaton) {
    var o = Object.create(this)

    // Needed for signaling transitions
    o.automaton = automaton

    // Used as EventEmitter implementation, since we can't just inherit from it.
    o.f = document.createDocumentFragment()

    return o
  },

  // Event emitter interface
  addListener: function(type, fn) {
    this.f.addEventListener(type, fn)
    return this },
  removeListener: function(type, fn) {
    this.f.removeEventListener(type, fn)
    return this },
  dispatch: function(event) {
    this.f.dispatchEvent(event)
    return this },

  enter: function() {
    this.dispatch(new CustomEvent('enter'))
    return this },
  leave: function() {
    this.dispatch(new CustomEvent('leave'))
    return this },

  // Add self transition
  on: function(condition, fn) {
    // Condition format is 'selector:event-type'
    condition = condition.split(':')
    var selector = condition[0]
    var type = condition[1]

    // When entering state, start listening for this condition on all elements
    // matching the selector
    this.addListener('enter', function() {
      // Use D3 to add the listener in order to have d3.event set up correctly
      // in the callback, and receiving data as argument.
      //
      // The cost is that since we do not take a namespace in, there can only be
      // one callback tied to a given type for the selected element.  So, the
      // latest specified prevails.
      d3.selectAll(selector)
        .on(type, fn)
    })

    // When leaving the state, stop listening
    this.addListener('leave', function() {
      d3.selectAll(selector)
        .on(type, null)
    })

    return this
  },

  // Add transition to another state
  to: function(state, condition, fn) {
    var self = this

    function cb(...args) {
      self.automaton.currentState.leave()
      if (fn) fn.apply(this, args)
      self.automaton.currentState = state
      self.automaton.currentState.enter()
    }

    return self.on(condition, cb)
  }
}
