// Try to be faster by keeping only one path at a time.
// Also, I'm wary of generator performance.

// Also, use a linear array instead of 2d arrays for representing the grid.

var Puzzle = {
  grid: null,
  width: 0,
  starts: null,
  goals: null,
  strict_edges: null,
  edge_lovers: null,
  incompatibles: null,

  new(str) {
    var o = Object.create(Puzzle)

    o.width = str[0].length
    o.grid = str.join('')

    o._parse_grid()

    return o
  },

  _parse_grid() {
    var g = this.grid
    this.starts = []
    this.goals = []
    this.strict_edges = []
    this.edge_lovers = []
    this.incompatibles = []

    const start_re = /O/g
    const goal_re = /A/g
    const strict_edges_re = /[=!]/g
    const edge_lovers_re = /[123]/g
    const incompatibles_re = /[a-z]/g

    var match

    while ((match = start_re.exec(g)) != null)
      this.starts.push(match.index)

    while ((match = goal_re.exec(g)) != null)
      this.goals.push(match.index)

    while ((match = strict_edges_re.exec(g)) != null)
      this.strict_edges.push(match.index)

    while ((match = edge_lovers_re.exec(g)) != null)
      this.edge_lovers.push([match[0], match.index])

    while ((match = incompatibles_re.exec(g)) != null)
      this.incompatibles.push([match[0], match.index])
  },
}

// Or use -w/-1/w/+1 to avoid move_in_dir
const UP    = 0b00,
      LEFT  = 0b01,
      DOWN  = 0b10,
      RIGHT = 0b11,
      STOP  = 4
// Opposite dir is xor 0b10

function is_solution(grid, puzzle) {
  var w = puzzle.width

  // Must pass through each strict edge
  if (!puzzle.strict_edges.every(e => grid[e] === 1))
    return false

  // Edge lovers must have exactly the number of edges around them
  if (!puzzle.edge_lovers.every(l => {
    // Count edges around this lover
    var p = l[1]
    var n = (grid[p+1] === 1)
          + (grid[p-1] === 1)
          + (grid[p+w] === 1)
          + (grid[p-w] === 1)
    return l[0] == n
  }))
    return false

  if (puzzle.incompatibles.length > 0) {

    // Now we need to distinguish connected components
    var ccs = connected_components(grid, puzzle.width)

    var cc, c, j, k, t
    var incompatible
    for (var i=0, l=ccs.length; i < l; ++i) {
      cc = ccs[i]

      // All incompatible cells in a component must be of the same type
      incompatible = null
      for (j=0, k=cc.length; j < k; ++j) {
        c = cc[j]
        t = puzzle.grid[c]
        if (/[a-z]/.test(t)) {
          if (incompatible == null)
            incompatible = t
          else if (incompatible != t)
            return false
        }
      }
    }
  }

  return true
}

// Return the connected components of the path, as arrays of cells
function connected_components(grid, width) {
  // Find components by flood filling the grid
  var components = []

  // Make a copy of the grid
  var g = grid.slice()
  var w = width

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
          bin.push(p)
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
}

const node_re = /[.OA]/;
const edge_re = /[-|=!]/;

function solve_faster(puzzle, found_solution) {
  var moves = []
  var pos = puzzle.starts[0]
  var goal = puzzle.goals[0]
  var puz = puzzle.grid
  var grid = new Array(puz.length)
  grid.fill(0)
  grid[pos] = 1
  var next

  // Start with something
  var dir = UP

  for (;;) {
    if (dir === STOP) {
      // No move to pop?  We are done
      if (moves.length === 0)
        break

      // Pop this move
      dir = moves.pop()
      grid[pos] = 0
      pos -= move_in_dir(dir)
      grid[pos] = 0
      pos -= move_in_dir(dir)

      // Next!
      dir += 1
      continue
    }

    // Can I go there?
    next = pos + move_in_dir(dir)

    // console.log(pos, next, moves)
    // print_grid(grid, puzzle.width)

    if (next >= 0 && next < puz.length // is inside
        && puz[next] !== ' '           // and there is an edge there
        && grid[next] === 0) {         // and we haven't been here already

      // Check node before going through
      next += move_in_dir(dir)

      if (next >= 0 && next < puz.length
          && puz[next] !== ' '  // there is a node there
          && grid[next] === 0) {

        // Ok, go there and mark the trail
        pos += move_in_dir(dir)
        grid[pos] = 1
        pos += move_in_dir(dir)
        grid[pos] = 1
        moves.push(dir)

        // Are we there yet?
        if (pos === goal && is_solution(grid, puzzle)) {
          found_solution(moves, grid, puzzle)
          // Stop looking further
          dir = STOP
        }
        else {
          // Keep pushing
          dir = UP
        }

        continue
      }
    }

    // Can't go there, try another direction
    dir += 1
  }

  function move_in_dir(dir) {
    var delta = dir & 0b01 ? 1 : puzzle.width
    return dir & 0b10 ? delta : -delta
  }
}

const puz1 = Puzzle.new([
  '.-.-.-A',
  '|   | !',
  '.-.-.-.',
  '|2| |3|',
  '.=.-.-.',
  '| | | |',
  'O .-.-O'
])

const puz5 = Puzzle.new([
  '.-.-.-.-.-A',
  '| | | | |3|',
  '.-.-.-.-.-.',
  '| | | | |1|',
  '.-.-.-.-.-.',
  '| |b| | |1|',
  '.-.-.-.-.-.',
  '|b|b|b| |1|',
  '.-.-.-.-.-.',
  '| | |a|a| |',
  'O-.-.-.-.-.',
])

var solutions = 0
solve_faster(puz5, function(moves, grid, puzzle) {
  console.log()
  print_grid(grid, puzzle.width)
  solutions += 1
})
console.log(`${solutions} solutions`)

function print_grid(grid, width) {
  for (var i=0, l=grid.length; i < l; i += width) {
    console.log(grid.slice(i, i + width).join(''))
  }
}
