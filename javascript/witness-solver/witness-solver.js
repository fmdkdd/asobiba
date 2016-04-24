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

function is_opposite_dir(a, b) {
  // Same axis, different direction
  return a ^ b === 0b10
}

const NONE       = 0b00,
      VERTICAL   = 0b01,
      HORIZONTAL = 0b10,
      BOTH       = 0b11

function mirror_dir(dir, mirror) {
  var vertical = dir & 0b01

  if (mirror === VERTICAL) {
    if (vertical) return dir ^ 0b10
    else return dir
  }

  else if (mirror === HORIZONTAL) {
    if (vertical) return dir
    else return dir ^ 0b10
  }

  else if (mirror === BOTH)
    return dir ^ 0b10

  // No mirror.  Why did you call me?
  return dir
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
      else
        Array.prototype.push.apply(next_states, expand(s))
    }

    states = next_states
  }
}

var Puzzle = {
  starts: null,
  goals: null,
  strict_edges: null,
  edge_lovers: null,
  incompatibles: null,
  height: 0,
  width: 0,
  grid: null,
  mirror: NONE,

  new(str, options) {
    options = options || {}

    var o = Object.create(Puzzle)

    o.mirror = options.mirror || NONE
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
    var incompatibles = []
    var height = this.grid.length
    var width = 0

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

      // Find incompatible cells
      while ((match = incompatibles_re.exec(line)) != null) {
        incompatibles.push([match[0], match.index, y])
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
    this.incompatibles = incompatibles
  },

  pos_type(pos) {
    return this.grid[this.height - 1 - pos[1]][pos[0]]
  },

  // Return the other start of the puzzle
  mirror_start(start) {
    // There should only be two starts, otherwise I really don't know what I'm
    // doing

    if (this.starts.length !== 2)
      throw new Error("Can't mirror more than 2 starts")

    // Return the other
    if (this.starts[0][0] === start[0]
        && this.starts[0][1] === start[1])
      return this.starts[1]
    else
      return this.starts[0]
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

    // Now we need to distinguish connected components
    var ccs = path.connected_components()

    var cc, c, j, k, t
    var incompatible
    for (var i=0, l=ccs.length; i < l; ++i) {
      cc = ccs[i]

      // All incompatible cells in a component must be of the same type
      incompatible = null
      for (j=0, k=cc.length; j < k; ++j) {
        c = cc[j]
        t = this.pos_type(c)
        if (t.search(incompatibles_re) > -1) {
          if (incompatible == null)
            incompatible = t
          else if (incompatible != t)
            return false
        }
      }
    }

    return true
  },
}

var Path = {
  puzzle: null,
  start: null,
  goal: null,
  current: null,
  grid: null,
  moves: null,

  // For mirror paths
  current_mirror: null,
  start_mirror: null,

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

    if (puzzle.mirror) {
      o.mirror_start = puzzle.mirror_start(start).slice()
      o.current_mirror = o.mirror_start
      o._set_grid_at(o.mirror_start, 2)
    }

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

    if (this.puzzle.mirror) {
      o.mirror_start = this.mirror_start.slice()
      o.current_mirror = this.current_mirror.slice()
    }

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

    // If mirror puzzle, need to walk from the other goal in the opposite
    // direction
    if (this.puzzle.mirror) {
      var rid = mirror_dir(dir, this.puzzle.mirror)

      // Move onto the edge
      move_in_dir(this.current_mirror, rid)
      this._set_grid_at(this.current_mirror, 2)

      // Move onto the node
      move_in_dir(this.current_mirror, rid)
      this._set_grid_at(this.current_mirror, 2)
    }

    this.moves.push(dir)
    return this
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
    if (this.been_through(pos)) return false

    // Same checks for mirror path
    if (this.puzzle.mirror) {
      var sop = this.current_mirror.slice()
      var rid = mirror_dir(dir, this.puzzle.mirror)

      move_in_dir(sop, rid)
      if (!this.puzzle.can_move_there(sop)) return false
      move_in_dir(sop, rid)
      if (!this.puzzle.can_move_there(sop)) return false

      if (this.been_through(sop)) return false
    }

    return true
  },

  // Are we there yet?
  is_solution() {
    var is_at_goal = this.current[0] == this.goal[0]
          && this.current[1] == this.goal[1]

    return is_at_goal && this.puzzle.does_satisfy_constraints(this)
  },

  // Return the connected components of the path, as arrays of cells
  connected_components() {
    // Find components by flood filling the grid
    var components = []

    // Make a copy of the grid
    var g = this.grid.slice()
    var w = this.grid.width
    var h = this.grid.height

    // Find a 0 (unoccupied position) and flood fill
    var idx, bin, p, x, y
    var queue = []
    while ((idx = g.indexOf(0)) > -1) {
      // Gather cells starting by idx
      queue.push(idx)
      bin = []

      // While there are unvisited cells
      while (queue.length > 0) {
        p = queue.shift()

        // Unvisited position
        if (g[p] === 0) {
          // Mark as visited
          g[p] = 9

          // and gather if cell
          x = p % w
          y = Math.floor(p / w)
          if (x % 2 == 1 && y % 2 == 1)
            bin.push([x, y])
        }

        // Add unvisited neighbors
        if (g[p - 1] === 0)
          queue.push(p - 1)
        if (g[p + 1] === 0)
          queue.push(p + 1)
        if (g[p - w] === 0)
          queue.push(p - w)
        if (g[p + w] === 0)
          queue.push(p + w)
      }

      // Done with this component
      components.push(bin)
    }

    return components
  },

  pretty_print() {
    var x, y, pos, mark
    var line = []
    var height = this.grid.height
    var width = this.grid.width

    console.log()
    for (y=height-1; y >= 0; --y) {
      line.length = 0
      for (x=0; x < width; ++x) {
        pos = [x,y]
        mark = this.been_through(pos)
        // Start or goal
        if (x == this.start[0] && y == this.start[1])
          line.push('O')
        else if (this.puzzle.pos_type(pos) === 'A')
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

  print_grid() {
    var x, y
    var height = this.grid.height
    var width = this.grid.width
    var line = []

    console.log()
    for (y=height-1; y >= 0; --y) {
      line.length = 0
      for (x=0; x < width; ++x) {
        line.push(this._get_grid_at([x,y]))
      }
      console.log(line.join(''))
    }
  },
}

// [a] -> [b] -> [a,b]
function zip(a, b) {
  var res = []
  a.forEach(aa => {
    b.forEach(bb => {
      res.push([aa, bb])
    })
  })
  return res
}

function solve_it(puzzle) {
  var s = solve(puzzle,
                p => zip(p.starts, p.goals).map(sg => Path.new(p, sg[0], sg[1])),
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
}

// Syntax:
// .     node
// O     starting node
// A     goal node
// -, |  edge
// =, !  edge that must be passed through (strict edge)
// 1,2,3 edge lovers cell constraint
// a-z   incompatible cells constraints

const starts_re = /O/g
const goals_re = /A/g
const strict_edges_re = /[=!]/g
const edge_lovers_re = /[123]/g
const incompatibles_re = /[a-z]/g

// Missing edges, strict edges and edge lovers
var puz1 = Puzzle.new([
  '.-.-.-A',
  '|   | !',
  '.-.-.-.',
  '|2| |3|',
  '.=.-.-.',
  '| | | |',
  'O .-.-O'
])

// Mirrors
var puz2 = Puzzle.new([
  '.-A-A-.',
  '|   | |',
  '.-.-.-.',
  '| | | |',
  '.-.-.-.',
  '| | |3|',
  'O-.-.-O'
], {mirror: HORIZONTAL})

var puz3 = Puzzle.new([
  'A-.-.-O',
  '|   | |',
  '.-.-.-.',
  '! |   |',
  '.-.-.-.',
  '| | |2|',
  'O-.-.-A'
], {mirror: BOTH})

// Incompatible cells
var puz4 = Puzzle.new([
  '.-.-.-.',
  '|a  |b|',
  '.-.-.-.',
  '! |   |',
  '.-.-.-.',
  '| |a|b|',
  'O-.-.-A'
])

solve_it(puz4)
