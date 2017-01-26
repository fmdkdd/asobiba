import React from 'react'
import './Box.css'

function BoxArea(props) {
  return (
    <svg width={props.width} height={props.height}>
      {props.children}
    </svg>
  )
}

class Box extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      focused: false,
    }

    this.mouseEnter = this.mouseEnter.bind(this)
    this.mouseLeave = this.mouseLeave.bind(this)
  }

  render() {
    let classes = ["box"]
    if (this.state.focused) {
      classes.push("focused")
    }

    const children = this.props.children.map((c, idx) =>
      <Cell key={c.props.label}
            x="0" y={idx * 40} label={c.props.label} value={c.props.value} />)

    return (
      <g className={classes.join(' ')}
         transform={`translate(${this.props.x} ${this.props.y})`}
         onMouseEnter={this.mouseEnter}
         onMouseLeave={this.mouseLeave}>
        {children}
      </g>
    )
  }

  mouseEnter() {
    this.highlight()
  }

  mouseLeave() {
    this.unhighlight()
  }


  highlight() {
    this.setState({
      focused: true,
    })
  }

  unhighlight() {
    this.setState({
      focused: false,
    })
  }
}

const REF = {}

function Cell(props) {
  return (
    <g className="cell" transform={`translate(${props.x} ${props.y})`}>
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

export {BoxArea, Box, Cell, REF}
