import React from 'react'

function withDrag(Comp) {
  return class WithDrag extends React.Component {
    constructor(props) {
      super(props)

      this.state = {
        x: 0,
        y: 0,
        mouseAtDragStartX: 0,
        mouseAtDragStartY: 0,
        posAtDragStartX: 0,
        posAtDragStartY: 0,
        dragging: false,
      }

      this.mouseDown = this.mouseDown.bind(this)
      this.mouseUp = this.mouseUp.bind(this)
      this.mouseMove = this.mouseMove.bind(this)
      this.dragStart = this.dragStart.bind(this)
      this.dragEnd = this.dragEnd.bind(this)
      this.drag = this.drag.bind(this)
    }

    render() {
      return (
        <g onMouseDown={this.mouseDown}
           transform={`translate(${this.state.x} ${this.state.y})`}>
          <Comp {...this.props} />
        </g>
      )
    }

    componentDidMount() {
      // Catch events on the document itself to avoid losing track of the
      // element when dragging it around if the cursor pass over other objects
      document.addEventListener('mousemove', this.mouseMove)
      document.addEventListener('mouseup', this.mouseUp)
    }

    componentWillUnmount() {
      document.removeEventListener('mousemove', this.mouseMove)
      document.removeEventListener('mouseup', this.mouseUp)
    }

    mouseDown(e) {
      e.preventDefault()
      this.dragStart(e.clientX, e.clientY)
    }

    mouseUp(e) {
      if (this.state.dragging) {
        this.dragEnd()
      }
    }

    mouseMove(e) {
      if (this.state.dragging) {
        this.drag(e.clientX, e.clientY)
      }
    }

    dragStart(x, y) {
      document.body.style.cursor = 'grabbing'

      this.setState(prev => ({
        mouseAtDragStartX: x,
        mouseAtDragStartY: y,
        posAtDragStartX: prev.x,
        posAtDragStartY: prev.y,
        dragging: true,
      }))
    }

    drag(x, y) {
      this.setState(prev => ({
        x: prev.posAtDragStartX + x - prev.mouseAtDragStartX,
        y: prev.posAtDragStartY + y - prev.mouseAtDragStartY,
      }))
    }

    dragEnd() {
      document.body.style.cursor = 'default'

      this.setState(prev => ({
        dragging: false,
      }))
    }
  }
}

export default withDrag
