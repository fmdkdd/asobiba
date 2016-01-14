document.addEventListener('DOMContentLoaded', init)

// TODO: when clicking on a circle that already has link starting from it,
// promote the link to temporary, but restore it on cancellation.

// REFACTOR: this is a simple automaton.
// init --click on circle--> begin-link
// begin-link --mousemove--> begin-link (and update path)
// begin-link --click on valid dst--> init (and erase tmp and install link)
// begin-link --click elsewhere--> init (and erase tmp)
// etc.
// It should be written as such for clarity.  As it stands, I'm already
// knee-deep in mixing up actions on state transitions and actions on state
// update.

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

  // XXX: FF has a strangely triggers a mouseenter when the mouse moves inside a
  // the rect, but *only when moving to the right*.
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
    // Get coordinates of src relative to the container SVG
    var bb = link_src.getBoundingClientRect()
    var svgbb = svg.node().getBoundingClientRect()
    var src_xy = [window.scrollX + bb.left - svgbb.left + bb.width / 2,
                  window.scrollY + bb.top - svgbb.top + bb.height / 2]
    // and coordinates of mouse relative to the same container
    var mouse = d3.mouse(svg.node())

    // Build line from src to mouse
    svg.append('line')
      .attr({class: 'tmp-link',
             stroke: 'black',
             x1: src_xy[0], y1: src_xy[1],
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

    // FIXME: In Chrome, cancel_link is called instead of end_link when clicking
    // on a node.
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

    // TODO: should return if the link already exists
    var dst = this

    // Otherwise, create permanent link between the two.
    var link = {from: src.__data__,
                to: dst.__data__}
    links.push(link)

    // Get top left coordinates of dst
    var bb = dst.getBoundingClientRect()
    var svgbb = svg.node().getBoundingClientRect()
    var dst_xy = [window.scrollX + bb.left - svgbb.left,
                  window.scrollY + bb.top - svgbb.top]

    svg.append('line')
      .attr({class: 'link',
             stroke: '#78b',
             x1: src_xy[0], y1: src_xy[1],
             x2: dst_xy[0], y2: dst_xy[1]})
  }
}
