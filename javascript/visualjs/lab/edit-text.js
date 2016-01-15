document.addEventListener('DOMContentLoaded', init)

// TODO: what if the user erases the whole content?

function init() {
  var svg = d3.select('#box-area')

  var node = svg.append('g')
        .attr('class', 'node')
        .attr('transform', 'translate(23 42)')
        .on('click', function() {
          console.log('click g')
        })

  node.append('rect')
    .attr({x: 10, y: 10,
           width: 200, height: 80,
           fill: 'white', stroke: 'black'})
    .on('click', function() {
      console.log('click rect')
    })

  node.append('text')
    .attr({dx: 20, dy: 60})
    .text('node')
    .on('click', edit_text)

  function edit_text() {
    // Add ability to edit SVG text element by
    // spawning a contenteditable div above the SVG text element.
    // When the text is saved, change the value of the SVG text element and
    // destroy the temporary div.

    // I tried to set document.body to contenteditable, which enables editing
    // the SVG text element directly, but the caret is mightily broken in
    // Firefox and Chrome.

    // If the contenteditable div proves unreliable across browsers, maybe use
    // an input text instead.

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
}
