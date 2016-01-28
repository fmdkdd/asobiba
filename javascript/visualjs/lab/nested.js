/*
 Understanding D3 nested selections for hierarchical data binding.

 Takeaways: I can use the update pattern for nested data.  I should use a key
 function for boxes.
 */

document.addEventListener('DOMContentLoaded', init)

var boxes = [
  {id: 0, x: 10, y: 10, things: ['a', 'b', 'c']},
  {id: 1, x: 90, y: 30, things: ['a']},
  {id: 2, x: 100, y: 90, things: ['b', 'c', 'e']}
]

var id = 3

function init() {
  d3.select('#add-circle')
    .on('click', function() {
      addBox(Math.random() * 500,
             Math.random() * 500)

      update()
    })

  update()
}

function update() {
  var svg = d3.select('#box-area')

  // BOXES
  var $boxes = svg.selectAll('.box')
        .data(boxes, d => d.id)

  // console.log('in: ', $boxes.enter().size(),
  //             'out: ', $boxes.exit().size())

  $boxes.exit().remove()

  $boxes.enter()
    .append('g')
    .attr({class: 'box'})
    .on('click', function(d) {
      removeBox(d)
      update()
    })

  $boxes
    .attr('transform', d => `translate(${d.x} ${d.y})`)


  // CELLS
  var $cells = $boxes.selectAll('.cell')
        .data(d => d.things)

  $cells.exit().remove()

  $cells.enter()
    .append('g')
    .attr('class', 'cell')
    .call(construct_cell)

  $cells
    .attr('transform', (d, i) => `translate(0 ${i * 20})`)
}

function construct_cell(selection) {
  selection
    .append('rect')
    .attr({
      fill: 'transparent',
      stroke: '#333',
      width: 50,
      height: 20,
    })

  selection
    .append('text')
    .attr({dy: 15, dx: 3})
    .text(d => d)
}


function addBox(x, y) {
  boxes.push({id: id++, x,y, things: ['d','e']})
}

function removeBox(box) {
  boxes.splice(boxes.indexOf(box), 1)
}
