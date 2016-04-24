// Puzzle solver for the game The Witness
//
// HERE BE SPOILERS
// You should try to figure out the meanings of the puzzle symbols for yourself
// first, because that's the fun part.

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

var Puzzle = {
  starts: null,
  goals: null,
  height: 0,
  width: 0,
  grid: null,

  new(str) {
    var o = Object.create(Puzzle)

    o.grid = str.slice()
    o._parse_grid()

    return o
  },

  _parse_grid() {
    var line, y, match

    var starts = []
    var goals = []
    var strict_edges = []
    var edge_lovers = []
    var height = this.grid.length
    var width = 0

    const starts_re = /O/g
    const goals_re = /A/g
    const strict_edges_re = /[=!]/g
    const edge_lovers_re = /[123]/g

    for (y=0; y < height; ++y) {
      line = this.grid[height - 1 - y]

      // Find starts
      while ((match = starts_re.exec(line)) != null) {
        starts.push([match.index, y])
      }

      // Find goals
      while ((match = goals_re.exec(line)) != null) {
        goals.push([match.index, y])
      }

      // Find strict edges
      while ((match = strict_edges_re.exec(line)) != null) {
        strict_edges.push([match.index, y])
      }

      // Find edge lovers
      while ((match = edge_lovers_re.exec(line)) != null) {
        edge_lovers.push([match[0], match.index, y])
      }

      // Save max width
      width = Math.max(line.length, width)
    }

    this.height = height
    this.width = width
    this.starts = starts
    this.goals = goals
    this.strict_edges = strict_edges
    this.edge_lovers = edge_lovers
  },

  // Is there a node or an edge there?
  // O(1)
  can_move_there(pos) {
    var x = pos[0]
    var y = pos[1]
    var height = this.height

    // Is inside the grid
    return x >= 0 && x < this.width && y >= 0 && y < this.height
    // and there is an edge or a node there
      && this.grid[height - 1 - y][x].search(/[-=|!OA.]/) > -1
  },

  // Check for additional puzzle constraints
  does_satisfy_constraints(path) {
    // Must pass through each strict edge
    if (!this.strict_edges.every(e => path.been_through(e)))
      return false

    // Edge lovers must have exactly the number of edges around them
    if (!this.edge_lovers.every(l => {
      // Count edges around this lover
      var x = l[1]
      var y = l[2]
      var n = path.been_through([x,y+1])
            + path.been_through([x,y-1])
            + path.been_through([x+1,y])
            + path.been_through([x-1,y])
      return l[0] == n
    }))
      return false

    return true
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
    // Move onto the edge
    move_in_dir(this.current, dir)
    this._set_grid_at(this.current, 1)

    // Move onto the node
    move_in_dir(this.current, dir)
    this._set_grid_at(this.current, 1)

    this.moves.push(dir)
    return this
  },

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

  // Would walking in this direction result in an invalid state?
  // Checks for
  is_valid(dir) {
    var pos = this.current.slice()
    move_in_dir(pos, dir)
    if (!this.puzzle.can_move_there(pos)) return false
    move_in_dir(pos, dir)
    if (!this.puzzle.can_move_there(pos)) return false

    // Don't backtrack
    return !this.been_through(pos)
  },

  // Are we there yet?
  is_solution() {
    var is_at_goal = this.current[0] == this.goal[0]
          && this.current[1] == this.goal[1]

    return is_at_goal && this.puzzle.does_satisfy_constraints(this)
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
        mark = this.been_through([x,y])
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

// Syntax:
// .     node
// O     starting node
// A     goal node
// -, |  edge
// =, !  edge that must be passed through (strict edge)
// 1,2,3 edge lovers cell constraint

var puz =  Puzzle.new([
  '.-.-.-A',
  '|   | !',
  '.-.=.-.',
  '|2| | |',
  '.=.-.-.',
  '| | ! |',
  'O .-.-O'
])

var s = solve(puz,
              p => [Path.new(p, p.starts[0], p.goals[0]),
                    Path.new(p, p.starts[1], p.goals[0])],
              s => s.is_solution(),
              s => {
                res = []
                ;[UP, RIGHT, DOWN, LEFT].forEach(dir => {
                  // Can we go there?
                  if (!s.is_valid(dir))
                    return

                  // Move in that direction
                  var ns = s.clone()
                  ns.move(dir)

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
