document.addEventListener('DOMContentLoaded', init)

// Model
var boxes = [{x: 23, y: 42}, {x: 80, y: 260}]
var links = new Map()

function init() {
  refresh()

  // d3.select('#box-area')
  //   .attr('class', 'ready')

  // Mouse enters box, grow circle, not when linking
  // circle.on('mouseenter', function() {
  //   d3.select(this)
  //     .transition()
  //     .duration(300)
  //     .ease('elastic')
  //     .attr('r', circle_grow_radius)
  // })
  // circle.on('mouseleave', function() {
  //   d3.select(this)
  //     .transition()
  //     .duration(300)
  //     .ease('elastic')
  //     .attr('r', circle_radius)
  // })

  // Click on circle begins linking
  // circle.on('click', function() {
  //   add_tmp_link
  // })
}

function refresh() {
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Enter/exit/update

  var $svg = d3.select('#box-area')

  var $boxes = $svg.selectAll('.box')
        .data(boxes)

  $boxes.exit().remove()

  var $box = $boxes.enter().append('g')
        .attr('class', 'box')

  var $rect = $box.append('rect')
        .attr({width: 200, height: 80})

  var $circle_radius = 15
  var $circle_grow_radius = 20
  var $circle = $box.append('circle')
        .attr({cx: 160, cy: 40, r: $circle_radius})

  $boxes
    .attr('transform', function(d) { return `translate(${d.x} ${d.y})`})

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Custom events dispatchers

  // Dispatch custom event on SVG root
  function dispatch(eventName, detail) {
    $svg.node().dispatchEvent(new CustomEvent(eventName, {detail: detail}))
  }

  // Can't decide where to store state for now, so use a getter/setter
  function state(stateName) {
    if (stateName == null)
      return $svg.attr('data-state')
    else
      $svg.attr('data-state', stateName)
  }

  // Initial state
  state('ready')

  $svg.on('click', function() {
    var target = d3.event.target

    // circle:click in ready state -> linking state & begin-link
    if (target.tagName === 'circle'
        && state() === 'ready') {
      dispatch('begin-link', {from: target})
      state('linking')
    }

    // rect:click in linking state -> link-box
    // and return to ready state
    // XXX: .box:click is what I want, but need to go up the tree
    if (target.tagName === 'rect'
        && state() === 'in-box') {
      dispatch('link-box', {box: target})
      state('ready')
    }

    if (target.tagName === 'svg'
        && state() === 'linking') {
      dispatch('cancel-link')
      state('ready')
    }
  })

  $svg.on('mouseover', function() {
    var target = d3.event.target

    // rect:mouseover in linking state -> select-box
    if (target.tagName === 'rect'
        && state() === 'linking') {
      dispatch('select-box', {box: target})
      // Have to transition to another state to stop the 'update-link' from
      // firing.  Alternately, could use a second bit of state, but as it's part
      // of the this interaction, it makes more sense to have it here.
      state('in-box')
    }
  })

  $svg.on('mouseout', function() {
    var target = d3.event.target

    // rect:mouseout in linking state -> unselect-box
    if (target.tagName === 'rect'
        && state() === 'in-box') {
      dispatch('unselect-box', {box: target})
      state('linking')
    }
  })

  d3.select(document).on('mousemove', function() {
    // document:mousemove in linking state -> update-link
    if (state() === 'linking') {
      dispatch('update-link', {endpoint: d3.mouse($svg.node())})
    }
  })

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Custom event listeners

  function on(eventName, callback) {
    $svg.node().addEventListener(eventName, callback)
  }

  on('begin-link', function(ev) {
    var from = ev.detail.from
    var from_bb = relativeBBox(from, $svg.node())
    var to = d3.mouse($svg.node())

    // Add link on top
    var link = $svg.insert('line', ':first-child')
    link.attr({class: 'tmp-link',
               x1: from_bb.cx, y1: from_bb.cy,
               x2: to[0], y2: to[1]})
  })

  on('update-link', function(ev) {
    var p = ev.detail.endpoint
    $svg.select('.tmp-link')
      .attr({x2: p[0], y2: p[1]})
  })

  on('select-box', function(ev) {
    var box = ev.detail.box

    // Snap tmp link to node corner
    var bb = relativeBBox(box, $svg.node())
    $svg.select('.tmp-link')
      .attr({x2: bb.x, y2: bb.y})

    box.classList.add('selected')
  })

  on('unselect-box', function(ev) {
    var box = ev.detail.box

    box.classList.remove('selected')
  })

  on('link-box', function(ev) {
    var box = ev.detail.box

    // Just remove the temporary status of the link
    $svg.select('.tmp-link')
      .attr('class', 'link')
  })

  on('cancel-link', function(ev) {
    $svg.select('.tmp-link')
      .remove()
  })



  // d3.selectAll('#box-area.ready circle')
  //   .on('mouseenter', function() {
  //     d3.select(this)
  //       .transition()
  //       .duration(300)
  //       .ease('elastic')
  //       .attr('r', $circle_grow_radius)
  //   })
  //   .on('mouseleave', function() {
  //     d3.select(this)
  //       .transition()
  //       .duration(300)
  //       .ease('elastic')
  //       .attr('r', $circle_radius)
  //   })
  //   .on('click', function() {
  //     $svg.attr('class', 'linking')
  //     refresh()
  //   })

  // d3.select('#box-area.linking')

  // d3.selectAll('#box-area.linking rect')
  //   .on('mouseenter', function() {
  //     d3.select(this)
  //       .transition()
  //       .duration(300)
  //       .ease('elastic')
  //       .attr('stroke', 'green')
  //   })
  //   .on('click', function() {
  //     $svg.attr('class', 'ready')
  //   })

  // d3.selectAll('#box-area.ready rect')


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
