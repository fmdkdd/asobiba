import React from 'react'
import * as Box from './Box'

class App extends React.Component {
  render() {
    const boxes = [
      {
        id: 0,
        x: 100,
        y: 30,
        cells: [
          {label: 'a', value: 1},
          {label: 'b', value: Box.REF},
        ],
      },
      {
        id: 1,
        x: 250,
        y: 80,
        cells: [
          {label: 'a', value: 1},
          {label: 'b', value: Box.REF},
        ],
      }
    ]

    const links = [
      {id: 0, start: 0, end: 1}
    ]

    return (
      <Box.BoxArea width="800" height="600"
                   boxes={boxes} links={links}>
      </Box.BoxArea>
    )
  }
}

export default App
