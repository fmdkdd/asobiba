/** Utilities for testing collisions between axis-aligned bounding boxes. */

/**
 * Return a point of coordinates (X,Y).
 */
function point(x, y) {
  return {x, y}
}

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
  return box1.x <= box2.x + box2.width
      && box1.x + box1.width >= box2.x
      && box1.y <= box2.y + box2.height
      && box1.y + box1.height >= box2.y
}


/**

 * Spatial hashing.

 We divide the game area in a grid.  Each objet is inserted in all the cells
 intersecting with its axis-aligned bounding box.

 A cell is a couple of coordinates {x, y}.  All cells have the same size.

 */

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
    return cell.x + '%' + cell.y
  },

  /** Return an array of the cells overlapping with the given axis-aligned
      bounding BOX. */
  cellsIntersectingWith(box) {
    var cells = []
    var start = this.cellFromPoint(box)
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

  /** Insert OBJECT in the grid, based on the coordinates of the axis-aligned
      bounding BOX.  As the bounding box can overlap multiple grid cells, we
      insert the object into all the intersecting cells. */
  insertObjectWithBoundingBox(obj, box) {
    for (var c of this.cellsIntersectingWith(box))
        this.insertObjectInCell(obj, c)
  },

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
