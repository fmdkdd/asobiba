import React from 'react'
import withDrag from './withDrag'
import './Box.css'

class BoxArea extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      boxes: this.props.boxes,
      links: this.props.links,
    }

    this.boxMoved = this.boxMoved.bind(this)
  }

  boxMoved(id, dx, dy) {
    this.setState(prev => {
      let b = prev.boxes.find(b => b.id === id)
      b.x += dx
      b.y += dy
      // Hmm, state is shallow merged, so we are in fact mutating the previous
      // boxes value.  This is fine, but it's not pure either
      return {boxes: prev.boxes}
    })
  }

  render() {
    const children = this.props.boxes.map(b =>
      <BoxWithDrag key={'b' + b.id}
                   x={b.x} y={b.y}
                   dragCallback={(dx, dy) => this.boxMoved(b.id, dx, dy)}>
        {b.cells.map(c => <Cell key={c.label} label={c.label} value={c.value} />)}
      </BoxWithDrag>
    ).concat(this.props.links.map(l => {
      let start = this.props.boxes.find(b => b.id === l.start)
      let end = this.props.boxes.find(b => b.id === l.end)

      return (
        <Line key={'l' + l.id}
              startX={start.x} startY={start.y}
              endX={end.x} endY={end.y} />
      )
    }))

    return (
      <svg width={this.props.width} height={this.props.height}>
        {children}
      </svg>
    )
  }
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

    let children = null;
    if (this.props.children) {
      children = this.props.children.map((c, idx) =>
        <Cell key={c.props.label}
              x="0" y={idx * 40} label={c.props.label} value={c.props.value} />)
    }

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

const BoxWithDrag = withDrag(Box)

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

function Line(props) {
  const path = `M ${props.startX} ${props.startY} L ${props.endX} ${props.endY}`

  return (
    <path className="link" d={path} />
  )
}

export {BoxArea, Box, Cell, REF, Line}
