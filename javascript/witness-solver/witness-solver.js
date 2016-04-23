// bb
// ||
// |axis
// direction
const LEFT  = 0b00,
      DOWN  = 0b01,
      RIGHT = 0b10,
      UP    = 0b11

function is_opposed_dir(a, b) {
  // Same axis, different direction
  return a ^ b === 0b10
}

function move_in_dir(pos, dir) {
  pos[dir & 0b1] = pos[dir & 0b1] + (dir & 0b10) - 1
  return pos
}

function* solve(problem, initial_states, is_goal, expand) {
  // Try valid moves until finding a solution or all possible solutions are
  // exhausted.

  var states = initial_states(problem)
  var next_states
  var s, i, l

  // While there are valid states, go on
  while ((l = states.length) > 0) {
    next_states = []

    // Test all states and expand
    for (i=0; i < l; ++i) {
      s = states[i]

      // Are we done yet?
      if (is_goal(s))
        yield s

      // Pump new states
      Array.prototype.push.apply(next_states, expand(s))
    }

    states = next_states
  }
}

// var s = solve({}, _ => [1], x => (x % 10) == 0, x => [x+2,x-1])

// console.log(s.next())
// console.log(s.next())

var puzzle = {
  width: 4,
  height: 4,
  start: [0,0],
  goal: [3,3],
}

var Path = {
  start: null,
  current: null,
  moves: null,
  positions: null,

  new(start) {
    var o = Object.create(Path)

    o.start = start.slice()
    o.current = start.slice()
    o.moves = []
    o.positions = [start.slice()]

    return o
  },

  clone() {
    var o = Object.create(Path)

    o.start = this.start.slice()
    o.current = this.current.slice()
    o.moves = this.moves.slice()
    o.positions = this.positions.slice()

    return o
  },

  move(dir) {
    move_in_dir(this.current, dir)
    this.moves.push(dir)
    this.positions.push(this.current.slice())
    return this
  },

  // last_move() {
  //   return this.moves[this.moves.length - 1]
  // },

  // Would going there would overlap a previous position?
  would_overlap(dir) {
    var pos = this.current.slice()
    move_in_dir(pos, dir)

    return this.been_through(pos)
  },

  // Didn't we pass through here already?
  been_through(pos) {
    return this.positions.some(p => p[0] == pos[0] && p[1] == pos[1])
  },

  is_inside(width, height) {
    var x = this.current[0]
    var y = this.current[1]

    return x >= 0 && x < width && y >= 0 && y < height
  },

  pretty_print_nodes(width, height) {
    var line = []
    var x, y

    console.log('-----')
    for (y=height-1; y >= 0; --y) {
      line.length = 0
      for (x=0; x < width; ++x) {
        if (this.been_through([x, y]))
          line.push('x')
        else
          line.push('o')
      }
      console.log(line.join(''))
    }
  },

  pretty_print(start, width, height) {
    // x-x-x = 3 nodes + 2 edges
    var out_w = 2 * width - 1
    var out_h = 2 * height - 1
    var output = new Array(out_w * out_h)
    output.fill(' ')

    // Construct the output by walking through the moves
    var pos = start.slice()
    output[pos[1] * out_w + pos[0]] = 'O'
    this.moves.forEach(m => {
      // Add edge
      move_in_dir(pos, m)
      output[pos[1] * out_w + pos[0]] = m & 0b01 ? '|' : '-'

      // Add node
      move_in_dir(pos, m)
      output[pos[1] * out_w + pos[0]] = 'X'
    })

    // And print the output
    var line = []
    var x, y

    console.log()
    for (y=out_h-1; y >= 0; --y) {
      line.length = 0
      for (x=0; x < out_w; ++x) {
        line.push(output[y * out_w + x])
      }
      console.log(line.join(''))
    }
  },
}

var s = solve(puzzle,
              _ => [Path.new(puzzle.start)],
              s => s.current[0] === puzzle.goal[0]
              && s.current[1] === puzzle.goal[1],
              s => {
                res = []
                ;[UP, RIGHT, DOWN, LEFT].forEach(dir => {
                  // Don't backtrack
                  if (s.would_overlap(dir))
                    return

                  // Move in that direction
                  var ns = s.clone()
                  ns.move(dir)

                  // Are we still inside the grid?
                  if (ns.is_inside(4, 4))
                    res.push(ns)
                })
                return res
              })

var i = 0
var v = s.next()
while (!v.done) {
  ++i
  v.value.pretty_print([0,0], 4, 4)
  v = s.next()
}
console.log('%d solutions', i)
