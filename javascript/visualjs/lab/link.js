document.addEventListener('DOMContentLoaded', init)

// TODO: when clicking on a circle that already has link starting from it,
// promote the link to temporary, but restore it on cancellation.

// TODO: should do nothing if a link already exists between targets.

function init() {

  var nodes = [{x: 23, y: 42}, {x: 80, y: 260}]
  var links = []


  var svg = d3.select('#box-area')

  // Build nodes
  var node = svg.selectAll('.node')
        .data(nodes)
        .enter().append('g')
        .attr('class', 'node')
        .attr('transform', function(d) { return `translate(${d.x} ${d.y})`})

  var rect = node.append('rect')
        .attr({width: 200, height: 80,
               fill: 'transparent', stroke: 'black'})

  var circle = node.append('circle')
        .attr({cx: 160, cy: 40, r: 15})


  // On click on circle, begin linking by overlaying a temporary path between
  // the link source and the mouse cursor.  When a second target is clicked, add
  // the link to the model, remove the overlay, and add the definitive link
  // path.  If the second click happens outside of a valid target, cancel
  // linking by destroying the overlay.

  var link_automaton = automaton.new(svg.node())
  var ready = state.new(link_automaton)
  var src_selected = state.new(link_automaton)
  var inside_node = state.new(link_automaton)

  var grow = animate_radius(20)
  var shrink= animate_radius(15)
  var stroke_green = animate_stroke('green')
  var stroke_black = animate_stroke('black')

  ready
    .addListener('enter', function() {
      // Reset data linked to the automaton
      link_automaton.data.link_src = null
      link_automaton.data.link_dst = null
      link_automaton.data.current_node = null
    })

    .to(inside_node, 'circle:click', function() {
      link_automaton.data.current_node = this.parentNode
      add_tmp_link.call(this)
    })

    .on('circle:mouseenter', grow)
    .on('circle:mouseleave', shrink)

  src_selected
    .on('body:mousemove', update_tmp_link)

    .to(inside_node, '.node:mouseenter', function() {
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
    .to(ready, '.node:click', function() {
      add_link.call(this)
      shrink.call(link_automaton.data.link_src)
    })

    .to(src_selected, '.node:mouseleave', function() {
      stroke_black.call(this.querySelector('rect'))
    })

  // The initial state
  link_automaton.enter(ready)



  function add_tmp_link() {
    link_automaton.data.link_src = this

    // Get coordinates relative to containing SVG
    var src_bb = relativeBBox(link_automaton.data.link_src, svg.node())
    // and coordinates of mouse relative to the same container
    var mouse = d3.mouse(svg.node())

    // Build line from src to mouse.  Add before other elements to the circles
    // appear on top of the line.
    svg.insert('line', ':first-child')
      .attr({class: 'tmp-link',
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
    var link = {from: src.__data__,
                to: dst.__data__}
    links.push(link)

    // Promote the temporary link to permanent
    svg.select('.tmp-link')
      .attr('class', 'link')

    // Also stroke the selected node back to black immediately.
    // XXX: This should be added by the animations, with an additional callback
    // on an existing transition.  But the automaton API does not allow that for
    // now.
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
