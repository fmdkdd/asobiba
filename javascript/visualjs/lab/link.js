document.addEventListener('DOMContentLoaded', init)

// TODO: when clicking on a circle that already has link starting from it,
// promote the link to temporary, but restore it on cancellation.

// TODO: should do nothing if a link already exists between targets.

// REFACTOR: this is a simple automaton.
// init --click on circle--> begin-link
// begin-link --mousemove--> begin-link (and update path)
// begin-link --click on valid dst--> init (and erase tmp and install link)
// begin-link --click elsewhere--> init (and erase tmp)
// etc.
// It should be written as such for clarity.  As it stands, I'm already
// knee-deep in mixing up actions on state transitions and actions on state
// update.

/*
 Complete (functional description of) automaton

 ready --click on circle--> select-dst
        |
        +- create temp line from circle to mouse

 select-dst --move mouse-> select-dst
             |
             +- set end point of temp line to mouse position

 select-dst --click on a free node-> ready
             |
             +- remove temp line
             +- add link between src and dst to model
             +- add link to view (update view)

 select-dst --click elsewhere-> ready
             |
             + remove temp line


 TODO: add highlighting above
 TODO: how to handle events?  Most nastiness of the code comes from the events
 handlers that must be added and removed on transitions.  They are not
 functional behavior, only needed by the implementation, so they should be
 handled directly by the automaton.
*/

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
  // the link source and the mouse cursor.  When a second (different) target is
  // clicked, add the link to the model, remove the overlay, and add the
  // definitive link path.  If the second click happens outside of a valid
  // target, cancel linking by destroying the overlay.

  restart_linking()

  var link_src
  function restart_linking() {
    link_src = null

    circle.on('click.link-create', begin_link)
    // Circle react on mouseover to signal that interaction is possible
    activate_circle_highlight()
  }

  function highlight_circle() {
    d3.select(this)
      .transition()
      .duration(300)
      .ease('elastic')
      .attr('r', 25)
  }

  function unhighlight_circle() {
    d3.select(this)
      .transition()
      .duration(300)
      .ease('elastic')
      .attr('r', 20)
  }

  function activate_circle_highlight() {
    circle.on('mouseenter.highlight-action', highlight_circle)
    circle.on('mouseleave.highlight-action', unhighlight_circle)
  }

  function deactivate_circle_highlight() {
    circle.on('mouseenter.highlight-action', null)
    circle.on('mouseleave.highlight-action', null)
  }

  function highlight_node() {
    d3.select(this).select('rect')
      .transition()
      .duration(300)
      .ease('elastic')
      .attr('stroke', 'green')
  }

  function unhighlight_node() {
    d3.select(this).select('rect')
      .transition()
      .duration(300)
      .ease('elastic')
      .attr('stroke', 'black')
  }

  function activate_node_highlight() {
    node.on('mouseenter.highlight-action', highlight_node)
    node.on('mouseleave.highlight-action', unhighlight_node)
  }

  function deactivate_node_highlight() {
    node.on('mouseenter.highlight-action', null)
    node.on('mouseleave.highlight-action', null)
  }

  function begin_link() {
    // No need to bubble
    d3.event.stopPropagation()

    link_src = this
    // Get relative to containing SVG
    var src_bb = relativeBBox(link_src, svg.node())
    // and coordinates of mouse relative to the same container
    var mouse = d3.mouse(svg.node())

    // Build line from src to mouse
    svg.append('line')
      .attr({class: 'tmp-link',
             stroke: 'black',
             x1: src_bb.cx, y1: src_bb.cy,
             x2: mouse[0], y2: mouse[1]})

    // to be updated whenever the mouse moves
    d3.select(document).on('mousemove.link-create', update_path)

    // Erase the listeners on circle
    circle.on('click.link-create', null)
    deactivate_circle_highlight()
    // and start listening on node instead
    node.on('click.link-create', end_link)
    activate_node_highlight()
    // and listen on the SVG as well for cancellation
    svg.on('click.link-create', cancel_link)
  }

  function update_path() {
    var mouse = d3.mouse(svg.node())
    svg.select('.tmp-link')
      .attr({x2: mouse[0], y2: mouse[1]})
  }

  function cancel_link() {
    // No need to bubble
    d3.event.stopPropagation()

    // Destroy temporary path and listeners
    svg.select('.tmp-link').remove()
    svg.on('mousemove.link-create', null)
    svg.on('click.link-create', null)
    node.on('click.link-create', null)
    deactivate_node_highlight()

    // Remove highlight on link_src circle
    unhighlight_circle.call(link_src)

    // Allow for linking to start afresh
    restart_linking()
  }

  function end_link() {
    var tmp_link = svg.select('.tmp-link')
    var src_xy = [tmp_link.attr('x1'),
                  tmp_link.attr('y1')]
    // Save before it is erased by cancel_link
    var src = link_src

    cancel_link()

    var dst = this

    // Remove highlight on node since the cursor is still in, and we removed the
    // listeners in cancel_link
    unhighlight_node.call(dst)

    // Otherwise, create permanent link between the two.
    var link = {from: src.__data__,
                to: dst.__data__}
    links.push(link)

    // Get top left coordinates of dst
    var dst_bb = relativeBBox(dst, svg.node())

    svg.append('line')
      .attr({class: 'link',
             stroke: '#78b',
             x1: src_xy[0], y1: src_xy[1],
             x2: dst_bb.x, y2: dst_bb.y})
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
