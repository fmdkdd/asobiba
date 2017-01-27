import React from 'react'

function withDrag(Comp) {
  return class WithDrag extends React.Component {
    constructor(props) {
      super(props)

      this.state = {
        dragStartX: 0,
        dragStartY: 0,
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
        <g onMouseDown={this.mouseDown}>
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

      this.setState({
        dragStartX: x,
        dragStartY: y,
        dragging: true,
      })
    }

    drag(x, y) {
      this.setState(prev => {
        const dx = x - prev.dragStartX
        const dy = y - prev.dragStartY

        if (this.props.dragCallback) {
          this.props.dragCallback(dx, dy)
        }

        return {
          dragStartX: x,
          dragStartY: y,
          dragging: true,
        }
      })
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
