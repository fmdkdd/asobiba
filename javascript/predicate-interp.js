// How to define an interpreter, object-style.  Essentially avoiding runtime
// inspection of object types, except for the atom.and and atom.or methods where
// only multi-methods would save us.

var expr = {
  and(b) { return AND(this, b) },
  or(b) { return OR(this, b) },
}

var true_ = {
  eval() { return this },
  eval2() { return true },
  and(b) { return b },
  or(b) { return this },
  toString() { return 'T' },
}

var false_ = {
  eval() { return this },
  eval2() { return false },
  and(b) { return this },
  or(b) { return b },
  toString() { return 'f' },
}

function atom(name) {
  return {__proto__: expr,
    eval() { return this },
    eval2() { return null },
    and(b) {
      if (b === true_) return this
      else if (b === false_) return false_
      else return this.__proto__.and.call(this, b)
    },
    or(b) {
      if (b === true_) return true_
      else if (b === false_) return this
      else return this.__proto__.or.call(this, b)
    },
    toString() { return name },
  }
}

// With multi-methods
// atom.or(self, true_): return this
// atom.or(self, false_): return false_
// atom.or(self, b): return this.__proto__.and.call(this, b)

function AND(left, right) {
  return {__proto__: expr,
    eval() { return left.eval().and(right.eval()) },
    eval2() { return left.eval2() && right.eval2() },
    toString() { return `${left} & ${right}` }
  }
}

function OR(left, right) {
  return {__proto__: expr,
    eval() { return left.eval().or(right.eval()) },
    eval2() { return left.eval2() || right.eval2() },
    toString() { return `${left} | ${right}` }
  }
}

AND(atom('a'), true_).eval().toString() //: "a"
AND(true_, true_).eval2() //: true
AND(atom('a'), AND(atom('b'), atom('c'))).eval().toString() //: "a & b & c"
AND(AND(atom('a'), atom('d')), AND(atom('b'), atom('c'))).eval().toString() //: "a & d & b & c"
AND(atom('a'), OR(atom('b'), atom('c'))).eval().toString() //: "a & b | c"
AND(OR(atom('b'), atom('c')), atom('a')).eval().toString() //: "b | c & a"
AND(atom('a'), OR(atom('b'), atom('c'))).eval2() //: null


function genExpr(depth) {
  var r = depth === 0 ? 3 : 5
  switch (Math.floor(Math.random() * r)) {
    case 0: return true_
    break
    case 1: return false_
    break
    case 2: return atom('a')
    break
    case 3: return AND(genExpr(depth-1), genExpr(depth-1))
    break
    case 4: return OR(genExpr(depth-1), genExpr(depth-1))
    break
  }
}

AND(atom('a'), OR(true_, atom('a'))).eval().toString() //: "a"
OR(AND(atom('a'), true_), atom('a')).eval().toString() //: "a | a"

var a = genExpr(13)
a.toString() //: "f"
a //: Object {eval:function eval,eval2:function eval2,and:function and,or:function or,toString:function toString}
a.eval().toString() //: "a & a & a | T & T & T | a"
