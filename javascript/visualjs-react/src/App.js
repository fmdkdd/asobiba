import React from 'react'
import * as Box from './boxes'
import withDrag from './withDrag'

const BoxWithDrag = withDrag(Box.Box)

class App extends React.Component {
  render() {
    return (
      <Box.BoxArea width="800" height="600">
        <BoxWithDrag x="100" y="30">
          <Box.Cell label="a" value="1" />
          <Box.Cell label="b" value={Box.REF} />
        </BoxWithDrag>
        <BoxWithDrag x="250" y="80">
          <Box.Cell label="a" value="1" />
          <Box.Cell label="b" value={Box.REF} />
        </BoxWithDrag>
      </Box.BoxArea>
    )
  }
}

export default App
