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

var Puzzle = {
  // width: 4,
  // height: 4,
  // start: [0,0],
  // goal: [3,3],

  grid: null,

  new(str) {
    var o = Object.create(Puzzle)

    o.grid = str.slice()
    o._parse_grid()

    return o
  },

  _parse_grid() {
    var line, x, y
    var starts = []
    var goals = []
    var height = this.grid.length
    var width = 0

    for (y=0; y < height; ++y) {
      line = this.grid[height - 1 - y]

      // Find starts
      x = line.indexOf('O')
      while (x > -1) {
        starts.push([x, y])
        x = line.indexOf('O', x + 1)
      }

      // Find goals
      x = line.indexOf('A')
      while (x > -1) {
        goals.push([x, y])
        x = line.indexOf('A', x + 1)
      }

      // Save max width
      width = Math.max(line.length, width)
    }

    this.starts = starts
    this.goals = goals
    this.height = height
    this.width = width
  },
}

var Path = {
  puzzle: null,
  start: null,
  goal: null,
  current: null,
  moves: null,

  new(puzzle, start, goal) {
    var o = Object.create(Path)

    o.puzzle = puzzle
    o.start = start
    o.goal = goal
    o.current = start.slice()
    o.moves = []
    o.grid = new Array(puzzle.width * puzzle.height)
    o.grid.width = puzzle.width
    o.grid.height = puzzle.height
    o.grid.fill(0)
    o._set_grid_at(start, 1)

    return o
  },

  clone() {
    var o = Object.create(Path)

    o.puzzle = this.puzzle
    o.start = this.start
    o.goal = this.goal
    o.current = this.current.slice()
    o.moves = this.moves.slice()
    o.grid = this.grid.slice()
    o.grid.width = this.grid.width
    o.grid.height = this.grid.height

    return o
  },

  _get_grid_at(pos) {
    return this.grid[pos[1] * this.grid.width + pos[0]]
  },

  _set_grid_at(pos, v) {
    this.grid[pos[1] * this.grid.width + pos[0]] = v;
    return this
  },

  // O(1)
  move(dir) {
    move_in_dir(this.current, dir)
    this._set_grid_at(this.current, 1)
    move_in_dir(this.current, dir)
    this._set_grid_at(this.current, 1)
    this.moves.push(dir)
    return this
  },

  // last_move() {
  //   return this.moves[this.moves.length - 1]
  // },

  // Would going there would overlap a previous position?
  // O(1)
  would_overlap(dir) {
    var pos = this.current.slice()
    move_in_dir(pos, dir)
    move_in_dir(pos, dir)

    return this.been_through(pos)
  },

  // Didn't we pass through here already?
  // O(1)
  been_through(pos) {
    return this._get_grid_at(pos) > 0
  },

  // Are we there yet?
  is_goal() {
    return this.current[0] == this.goal[0] && this.current[1] == this.goal[1]
  },

  // O(1)
  is_inside(width, height) {
    var x = this.current[0]
    var y = this.current[1]

    return x >= 0 && x < width && y >= 0 && y < height
  },

  pretty_print() {
    var x, y, mark
    var line = []
    var height = this.grid.height
    var width = this.grid.width

    console.log()
    for (y=height-1; y >= 0; --y) {
      line.length = 0
      for (x=0; x < width; ++x) {
        mark = this._get_grid_at([x,y]) > 0
        // Start or goal
        if (x == this.start[0] && y == this.start[1])
          line.push('O')
        else if (x == this.goal[0] && y == this.goal[1])
          line.push('A')

        // Edges
        else if (x % 2 == 1 && mark)
          line.push('-')
        else if (y % 2 == 1 && mark)
          line.push('|')

        // Nodes
        else if (mark)
          line.push('X')

        // Neither edge nor node
        else
          line.push(' ')
      }
      console.log(line.join(''))
    }
  },
}


var puz =  Puzzle.new([
  '.-.-.-A',
  '| | | |',
  '.-.-.-.',
  '| | | |',
  '.-.-.-.',
  '| | | |',
  'O-.-.-O'
])

console.log(puz)

var s = solve(puz,
              p => [Path.new(p, p.starts[1], p.goals[0])],
              s => s.is_goal(),
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
                  if (ns.is_inside(s.puzzle.width, s.puzzle.height))
                    res.push(ns)
                })
                return res
              })

var i = 0
var v = s.next()
while (!v.done) {
  ++i
  v.value.pretty_print()
  v = s.next()
}
console.log('%d solutions', i)
