document.addEventListener("DOMContentLoaded", init)

// Feature 1: draggable boxes
// TODO: Feature 2: link boxes with arcs

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
  parent: null,                 // prototype link
  properties: [                 // list of properties
    {label: 'a', value: 1},
    {label: 'b', value: ref},
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
  var inside_node = state.new(link_automaton)

  var grow = animate_radius(10)
  var shrink= animate_radius(4)
  var stroke_green = animate_stroke('#4ECDC4')
  var stroke_black = animate_stroke('#556270')

  ready
    .addListener('enter', function() {
      // Reset data linked to the automaton
      link_automaton.data.link_src = null
      link_automaton.data.link_dst = null
      link_automaton.data.current_node = null
    })

    .to(inside_node, '.ref:click', function() {
      link_automaton.data.current_node = this.parentNode
      add_tmp_link.call(this)
    })

    .on('.ref:mouseenter', grow)
    .on('.ref:mouseleave', shrink)

  src_selected
    .on('body:mousemove', update_tmp_link)

    .to(inside_node, '.cell:mouseenter', function() {
      link_automaton.data.current_node = this
    })

  // Clicking anywhere else cancels linking
    .to(ready, 'svg:click', function() {
      remove_tmp_link.call(this)

      // When the link is added or canceled, shrink the circle back.  This is needed
      // because we voluntarily leave the first selected circle in the grow state.
      shrink.call(link_automaton.data.link_src)
    })

  inside_node
    .addListener('enter', select_node)
    .addListener('leave', function() {
      link_automaton.data.current_node = null
    })

  // Clicking a node installs the link from the source to it
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
      link = svg.insert('line', ':first-child')
    }

    link.attr({class: 'tmp-link',
             x1: src_bb.cx, y1: src_bb.cy,
             x2: mouse[0], y2: mouse[1]})

    // No bubbling necessary
    d3.event.stopPropagation()
  }

  function update_tmp_link() {
    var mouse = d3.mouse(svg.node())
    svg.select('.tmp-link')
      .attr({x2: mouse[0], y2: mouse[1]})
  }

  function select_node() {
    var node = link_automaton.data.current_node
    stroke_green.call(node.querySelector('rect'))

    // Snap tmp link to node corner
    var bb = relativeBBox(node, svg.node())
    svg.select('.tmp-link')
      .attr({x2: bb.x, y2: bb.y})
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

    // Also stroke the selected node back to black immediately
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
        d3.select(this)
          .attr('transform', `translate(${d.x = d3.event.x} ${d.y = d3.event.y})`)

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
