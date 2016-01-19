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
               fill: 'white', stroke: 'black'})

  var circle = node.append('circle')
        .attr({cx: 160, cy: 40, r: 20})


  // On click on circle, begin linking by overlaying a temporary path between
  // the link source and the mouse cursor.  When a second target is clicked, add
  // the link to the model, remove the overlay, and add the definitive link
  // path.  If the second click happens outside of a valid target, cancel
  // linking by destroying the overlay.

  var link_automaton = automaton.new(svg.node())
  var ready = state.new(link_automaton)
  var selectDst = state.new(link_automaton)

  ready
    .to(selectDst, 'circle:click', add_tmp_link)

  selectDst
    .on('body:mousemove', update_tmp_link)

  // Clicking a node installs the link between from the source to it
    .to(ready, '.node:click', add_link)

  // Clicking anywhere else cancels linking
    .to(ready, 'svg:click')

  selectDst.addListener('leave', remove_tmp_link)

  ready
    .addListener('enter', function() {
      // The first circle clicked
      link_automaton.data.link_src = null
      link_automaton.data.link_dst = null
    })

  function add_tmp_link() {
    link_automaton.data.link_src = this

    // Get coordinates relative to containing SVG
    var src_bb = relativeBBox(link_automaton.data.link_src, svg.node())
    // and coordinates of mouse relative to the same container
    var mouse = d3.mouse(svg.node())

    // Build line from src to mouse
    svg.append('line')
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

    // Get coordinates of src and dst for the resilient link
    var src_bb = relativeBBox(src, svg.node())
    var dst_bb = relativeBBox(dst, svg.node())

    svg.append('line')
      .attr({class: 'link',
             x1: src_bb.cx, y1: src_bb.cy,
             x2: dst_bb.x, y2: dst_bb.y})

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

  var grow = animate_radius(25)
  var shrink= animate_radius(20)

  function animate_stroke(color) {
    return function() {
      d3.select(this)
        .transition()
        .duration(300)
        .ease('elastic')
        .attr('stroke', color)
    }
  }

  var stroke_green = animate_stroke('green')
  var stroke_black = animate_stroke('black')

  // Additional transitions for the automaton
  ready
    .on('circle:mouseenter', grow)
    .on('circle:mouseleave', shrink)

  selectDst
    .on('.node:mouseenter', function() {
      stroke_green.call(this.querySelector('rect')) })
    .on('.node:mouseleave', function() {
      stroke_black.call(this.querySelector('rect')) })

  // When the link is added or canceled, shrink the circle back.  This is needed
  // because we voluntarily leave the first selected circle in the grow state.
  selectDst.addListener('leave', function() {
    shrink.call(link_automaton.data.link_src)
  })

  // The initial state
  link_automaton.currentState = ready
  link_automaton.currentState.enter()
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
  addListener: function(type, fn) { this.f.addEventListener(type, fn) },
  removeListener: function(fn) { this.f.removeEventListener(fn) },
  dispatch: function(event) { this.f.dispatchEvent(event) },

  enter: function() { this.dispatch(new CustomEvent('enter')) },
  leave: function() { this.dispatch(new CustomEvent('leave')) },

  // Add self transition
  on: function(condition, fn) {
    // Condition format is 'selector:event-type'
    condition = condition.split(':')
    var selector = condition[0]
    var type = condition[1]

    // When entering state, start listening for this condition on all elements
    // matching the selector
    this.addListener('enter', function() {
      // XXX: Assuming there will only be at most one transition of this type
      // for the selected elements.  Otherwise, the latest specified prevails.
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
// container.  The bounding box also accounts for window scrolling.  The
// bounding box is an object with the properties:
// top (alias y), left (alias x), down, right, width, height,
// cx (x of center), cy (y of center)
function relativeBBox(elem, container) {
  var bb = elem.getBoundingClientRect()
  var ref = container.getBoundingClientRect()

  var left = window.scrollX + bb.left - ref.left
  var top = window.scrollY + bb.top - ref.top

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
