import React, { Component } from 'react'
import './App.css'

class App extends Component {
  render() {
    return (
      <svg width="500" height="500">
        <Box x="100" y="30"/>
        <Box x="250" y="80"/>
      </svg>
    )
  }
}

const REF = {};

class Box extends Component {
  constructor(props) {
    super(props)

    this.state = {
      x: parseInt(this.props.x, 10),
      y: parseInt(this.props.y, 10),
      dragStartX: 0,
      dragStartY: 0,
      originX: 0,
      originY: 0,
      isFocused: false,
      isDragged: false,
    }

    this.mouseEnter = this.mouseEnter.bind(this)
    this.mouseLeave = this.mouseLeave.bind(this)
    this.mouseDown = this.mouseDown.bind(this)
    this.mouseUp = this.mouseUp.bind(this)
    this.mouseMove = this.mouseMove.bind(this)
  }

  render() {
    let className = "box";
    if (this.state.isFocused) {
      className += " focused"
    }

    return (
      <g className={className}
         transform={translate(this.state.x, this.state.y)}
         onMouseEnter={this.mouseEnter}
         onMouseLeave={this.mouseLeave}
         onMouseDown={this.mouseDown}>
        <Cell x="0" y="0" label="a" value="1" />
        <Cell x="0" y="40" label="b" value={REF} />
      </g>
    )
  }

  mouseEnter() {
    this.highlight()
  }

  mouseLeave() {
    this.unhighlight()
  }

  mouseDown(e) {
    e.preventDefault()
    this.dragStart(e.clientX, e.clientY)
  }

  mouseUp(e) {
    this.dragEnd(e.clientX, e.clientY)
  }

  mouseMove(e) {
    if (this.state.isDragged) {
      this.dragMove(e.clientX, e.clientY)
    }
  }

  highlight() {
    this.setState({
      isFocused: true,
    })
  }

  unhighlight() {
    this.setState({
      isFocused: false,
    })
  }

  dragStart(x, y) {
    // Catch events on the document itself to avoid losing track of the
    // element when dragging it around
    document.addEventListener('mousemove', this.mouseMove)
    document.addEventListener('mouseup', this.mouseUp)
    document.body.style.cursor = 'grabbing'

    this.setState((prev) => ({
      dragStartX: x,
      dragStartY: y,
      originX: prev.x,
      originY: prev.y,
      isDragged: true,
    }))
  }

  dragMove(x, y) {
    this.setState((prev) => ({
      x: prev.originX + x - prev.dragStartX,
      y: prev.originY + y - prev.dragStartY,
    }))
  }

  dragEnd(x, y) {
    if (this.state.isDragged) {
      document.body.style.cursor = 'default'

      this.setState((prev) => ({
        x: prev.originX + x - prev.dragStartX,
        y: prev.originY + y - prev.dragStartY,
        isDragged: false,
      }))
    }
  }
}

function Cell(props) {
  return (
    <g className="cell" transform={translate(props.x, props.y)}>
      <rect width="100" height="40"></rect>
      <text dx="5" dy="25">{props.label}</text>
      <line x1="70" y1="0" x2="70" y2="40"></line>
      {props.value === REF ? (
        <circle className="ref" cx="85" cy="20" r="4"></circle>
      ) : (
        <text dx="80" dy="25">{props.value}</text>
      )}
    </g>
  )
}

function translate(x, y) {
  return `translate(${x} ${y})`
}

export default App;
