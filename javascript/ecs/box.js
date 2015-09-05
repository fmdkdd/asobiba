
// Axis-aligned bounding boxes
// Utilities for testing collisions between axis-aligned bounding boxes.

// [[file:box.org::*Axis-aligned%20bounding%20boxes][Axis-aligned\ bounding\ boxes:1]]

/**
 * Create a box with from the coordinates of its TOP_LEFT corner, its
 * WIDTH and its HEIGHT.
 */
function box(top_left, width, height) {
  return {x: top_left.x, y: top_left.y, width, height}
}

/**
 * Return the center point of BOX.
 */
function box_center(box) {
  return point(box.x + box.width  / 2,
               box.y + box.height / 2)
}

/**
 * Return all the corners of BOX as an array of points.
 */
function box_corners(box) {
  return [
    point(box.x, box.y),
    point(box.x + box.width, box.y),
    point(box.x + box.width, box.y + box.height),
    point(box.x, box.y + box.height),
  ]
}

/**
 * Return true if and only if POINT lies inside BOX.
 */
function is_point_inside_box(point, box) {
  return point.x >= box.x && point.x <= box.x + box.width
      && point.y >= box.y && point.y <= box.y + box.height
}

/**
 * Return true if and only if BOX1 and BOX2 have some overlap.
 */
function do_boxes_collide(box1, box2) {
  // Consequence of the Separation Axis Theorem (SAT): if the two boxes overlap,
  // their projections on the two axes overlap as well.  Conversely, if there is
  // a gap in one axis, then the boxes do not overlap.
  return box1.x <= box2.x + box2.width && box1.x + box1.width >= box2.x
      && box1.y <= box2.y + box2.height && box1.y + box1.height >= box2.y
}

// Axis-aligned\ bounding\ boxes:1 ends here

// SAT collision detection for convex polygons
// :PROPERTIES:
// :CUSTOM_ID: SAT
// :END:

// [[file:box.org::*SAT%20collision%20detection%20for%20convex%20polygons][SAT\ collision\ detection\ for\ convex\ polygons:1]]

/**
 * Return true if convex polygons POLY1 and POLY2 overlap.  Both arguments are
 * arrays of vertices describing the polygons.  The array of vertices is open
 * (i.e., there are as many points in the array as there are sides to the
 * polygon).
 */
function do_polygons_collide(poly1, poly2) {
  // Use the SAT theorem for determining if the two convex polygons collide.
  return no_separation_axis(poly1, poly2) && no_separation_axis(poly2, poly1)
}

/**
 * Return true if there is no separation axis between POLY1 and POLY2, checking
 * only for the axes of POLY1.
 *
 * Used by do_polygons_collide.
 */
function no_separation_axis(poly1, poly2) {
  for (var axis of get_axes(poly1)) {
    var p1 = project(poly1, axis)
    var p2 = project(poly2, axis)
    if (!overlap(p1, p2))
      return false
  }

  return true
}

/**
 * Return the list of axes of POLY used to find a separation axis by
 * no_separation_axis.  The list is returned as an array of vectors.
 */
function get_axes(poly) {
  // For each edge of the polygon, return its normal.

  var axes = []
  for (var i = 0; i < poly.length; ++i) {
    var p0 = poly[i]
    var p1 = poly[(i + 1) % poly.length]
    var edge = vec_minus(p0, p1)
    var normal = vec_unit(vec_perp(edge))
    // We return the normalized axis, though it is only necessary if we wish to
    // find the minimum translation between the polygons.
    axes.push(normal)
  }

  return axes
}

/**
 * Return the interval obtained by projecting POLY onto AXIS.
 * The interval is an object {min, max}.
 */
function project(poly, axis) {
  // To project the polygon onto the axis, dot product each vertex with the
  // axis, and keep the min and max values.

  var min = +Infinity
  var max = -Infinity

  for (var vert of poly) {
    var p = vec_dot(axis, vert)
    if (p < min) min = p
    if (p > max) max = p
  }

  return {min, max}
}

/**
 * Return true if projections PROJ1 and PROJ2 overlap.
 */
function overlap(proj1, proj2) {
  // Compare bounds of interval.  Single axis version of do_boxes_collide.
  return proj1.min <= proj2.max && proj1.max >= proj2.min
}

// SAT\ collision\ detection\ for\ convex\ polygons:1 ends here

// Hitbox projection
// Useful for projecting hitboxes of rotating, moving objects which store their
// hitboxes as relative coordinates.

// [[file:box.org::*Hitbox%20projection][Hitbox\ projection:1]]

/**
 * Rotate POLY along ANGLE, then translate it along VEC, and return the result
 * as a new polygon.
 */
function adjust_hitbox(poly, vec, angle) {
  var p = []
  for (var v of poly) {
    p.push(vec_plus(vec_rotate(v, angle), vec))
  }
  return p
}

// Hitbox\ projection:1 ends here

// Spatial hashing
// Spatial hashing helps avoiding the exponential complexity of checking all
// objects against each other for collisions.  Instead, objects are checked for
// collisions only if they reside in the same spatial hash cell.  This is called a
// /broad phase collision detection/.

// We divide the game area in a grid of cells.  Each object is inserted in all the
// cells intersecting with its axis-aligned bounding box.

// A cell is a couple of coordinates {x, y}.  All cells have the same size.

// Here the area is divided into four cells of same size.  Boxes in cells 1 and 2
// cannot collide, so they are not checked for collisions.  Only one test between
// the two green boxes is needed, instead of 3.

// [[file:spatial-hash1.png]]

// Choosing the cell size is a compromise: small cells will allocate more memory,
// but lookups in each cell will be faster.  However, a global lookup of collisions
// through all the cells will not benefit much if objects are duplicated in many
// cells.  Large cells may contain too many objects, and thus we may lose the
// benefit of spatial hashing.

// Ideally objects should appear in the fewest cells possible.  This implies that
// cells should be larger than the average object, but not too much.  Assuming the
// objects do not deviate wildly from the average, between 1 and 2 times the
// average object size is a good value for the cell size.

// Positions of objects in the hash are not tracked: clients of the hash should
// remove and reinsert objects that move.

// Objects that do not move, but can collide, need to be inserted in the hash only
// once.

// Spatial hashing is best for game areas without wild variations of object
// density.  Alternatives that might be better suited to these variations:
// quad-trees or r-trees.

// [[file:box.org::*Spatial%20hashing][Spatial\ hashing:1]]

var emptySet = new Set()

var spatialHash = {
  new(cellSize) {
    return {
      __proto__: this,
      cellSize,
      map: new Map(),
    }
  },

  /** Return the cell coordinates of POINT. */
  cellFromPoint(point) {
    var x = Math.floor(point.x / this.cellSize)
    var y = Math.floor(point.y / this.cellSize)
    return {x, y}
  },

  /** Return the hash value of CELL, used as a key into the grid map. */
  hashCell(cell) {
    return `${cell.x}%${cell.y}`
  },

  /** Return an array of the cells overlapping with the given axis-aligned
      bounding BOX. */
  cellsIntersectingWith(box) {
    var cells = []
    var start = this.cellFromPoint(point(box.x, box.y))
    var end = this.cellFromPoint({x: box.x + box.width,
                                  y: box.y + box.height})

    for (var x = start.x; x <= end.x; ++x)
      for (var y = start.y; y <= end.y; ++y)
        cells.push({x,y})

    return cells
  },

  insertObjectInCell(obj, cell) {
    var h = this.hashCell(cell)
    if (!this.map.has(h))
      this.map.set(h, new Set())

    this.map.get(h).add(obj)
  },

// Spatial\ hashing:1 ends here

// #+RESULTS:

// Here the green box overlaps cells 1 and 2, so it has to be inserted into both.
// The worst case is the orange box which must be inserted into four cells.

// [[file:spatial-hash2.png]]

// [[file:box.org::*Spatial%20hashing][Spatial\ hashing:1]]

/** Insert OBJECT in the grid, based on the coordinates of the axis-aligned
    bounding BOX. */
insertObjectWithBoundingBox(obj, box) {
      for (var c of this.cellsIntersectingWith(box))
          this.insertObjectInCell(obj, c)
    },

// Spatial\ hashing:1 ends here

// [[file:box.org::*Spatial%20hashing][Spatial\ hashing:1]]

/** Return the set of objects present in CELL. */
  objectsInCell(cell) {
    return this.map.get(this.hashCell(cell))
           || emptySet
  },

  /** Return the set of objects present in the cell of the grid POINT is
      in. */
  objectsNearPoint(point) {
    return this.map.get(this.hashCell(this.cellFromPoint(point)))
           || emptySet
  },

  /** Return the set of objects present in all the cells overlapping with the
      axis-aligned bounding BOX. */
  objectsNearBoundingBox(box) {
    var objs = new Set()

    for (var c of this.cellsIntersectingWith(box))
      for (var o of this.map.get(this.hashCell(c)))
        objs.add(o)

    return objs
  },

  /** Remove all objects in CELL. */
  clearCell(cell) {
    this.map.get(this.hashCell(cell)).clear()
  },

  /** Remove all objects from the grid.  Cells are not deallocated. */
  clearAllCells() {
    for (var kv of this.map)
      kv[1].clear()
  },

  printStats() {
    var avg = 0
    for (var kv of this.map)
      avg += kv[1].size
    avg /= this.map.size

    console.log('Allocated cells', this.map.size)
    console.log('Average objects per cell', avg)
  }
}

// Spatial\ hashing:1 ends here

// TODO Constructors
// A vector is a couple of coordinates (x,y) which indicates its displacement from
// the origin (0,0).

// We store the coordinates in a JavaScript object with two properties =x= and
// =y=.

// #+NAME: vector

// [[file:box.org::*Constructors][vector]]

/** Return a vector of coordinates (X,Y). */
function vector(x, y) { return {x, y}}

// vector ends here

// A point is also a couple of coordinates (x,y).  As such, the constructor is
// identical to that of a vector.

// The resulting objects are exactly the same, so we can use both constructors
// interchangeably.  However, we try use the point constructor when it is more
// semantically appropriate (e.g., [[is_point_inside_box][is_point_inside_box]]).

// #+NAME: point

// [[file:box.org::*Constructors][point]]

/** Return a point of coordinates (X,Y). */
function point(x, y) { return {x, y}}

// point ends here

// TODO Adding and subtracting vectors
// To add two vectors, add their coordinates.

// [[file:vec_plus.png]]

// This is used for translating a vector.

// #+NAME: vec_plus

// [[file:box.org::*Adding%20and%20subtracting%20vectors][vec_plus]]

function vec_plus(u, v) {
  return point(u.x + v.x, u.y + v.y)
}

// vec_plus ends here

// Subtraction is the same as adding a negated vector.  We redefine it more
// directly rather than using [[vec_dot][vec_dot]].

// Subtraction is useful for finding the vector between two points:


// #+NAME: vec_minus

// [[file:box.org::*Adding%20and%20subtracting%20vectors][vec_minus]]

function vec_minus(u, v) {
  return point(u.x - v.x, u.y - v.y)
}

// vec_minus ends here

// Scaling a vector
// To scale a vector, multiply each of its coordinates.

// #+NAME: vec_mult

// [[file:box.org::*Scaling%20a%20vector][vec_mult]]

function vec_mult(v, s) {
  return point(v.x * s, v.y * s)
}

// vec_mult ends here

// TODO Rotating a vector
// #+NAME: vec_rotate

// [[file:box.org::*Rotating%20a%20vector][vec_rotate]]

function vec_rotate(v, a) {
  var cos = Math.cos(a)
  var sin = Math.sin(a)
  return point(v.x * cos - v.y * sin,
               v.x * sin + v.y * cos)
}

// vec_rotate ends here

// TODO Dot product
// #+NAME: vec_dot

// [[file:box.org::*Dot%20product][vec_dot]]

function vec_dot(u, v) {
  return u.x * v.x + u.y * v.y
}

// vec_dot ends here

// Vector length
// Vector length, or magnitude, is the distance between the vector coordinates and
// the origin.

// To obtain it, we apply Pythagoras’ theorem.

// [[file:vec_length.png]]

// #+NAME: vec_length

// [[file:box.org::*Vector%20length][vec_length]]

function vec_length(v) {
  return Math.sqrt(v.x*v.x + v.y*v.y)
}

// vec_length ends here

// Unit vector
// Unit vectors are vectors of length 1.  Trivial examples are (0,1) and (1,0).

// We /normalize/ a vector when we adjust its magnitude to 1 (without changing its
// direction).

// To normalize a vector, we scale it by the inverse of its magnitude.

// #+NAME: vec_unit

// [[file:box.org::*Unit%20vector][vec_unit]]

function vec_unit(v) {
  var l = vec_length(v)
  return point(v.x / l, v.y / l)
}

// vec_unit ends here

// Normal vector
// Rotating a vector by 90° gives its /normal/ vector, which is perpendicular.

// [[file:vec_perp.png]]

// The normal vector is useful for finding the axis of projection in the [[#SAT][SAT
// algorithm]].

// As this is a common operation, we do not want to use [[vec_rotate][vec_rotate]].
// Instead, we can plug the values of cos and sin for 90°.

// : cos(90°) = 0
// : sin(90°) = 1

// #+NAME: vec_perp

// [[file:box.org::*Normal%20vector][vec_perp]]

function vec_perp(v) {
  return point(-v.y, v.x)
}

// vec_perp ends here
