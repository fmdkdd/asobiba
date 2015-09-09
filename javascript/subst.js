/* eslint-disable */

function app(e1, e2) { return {type: 'app', e1, e2, toString() { return `(${e1} ${e2})`}}}
function fun(x, e) { return {type: 'fun', x, e, toString() { return `(\\${x}. ${e})`}}}
function v(x) { return {type: 'v', x, toString() { return x}}}
function c(e) { return {type: 'c', e, toString() { return e}}}
function subst(e1, x, e2) { return {type: 'subst', e1, x, e2, toString() { return `(${e1} [${x}=${e2}])`}}}
function R(e1, x, e2) { return {type: 'R', e1, x, e2, toString() { return `(${e1} <${x}=${e2}>)`}}}

var depth = 0

function indent(depth) {
  var w = ''
  for (var i = 0; i < depth; ++i) {w += '  '}
  return w
}

function interp(node) {
  var f = nodes[node.type]
  
  if (f) {
    console.log(`${indent(depth++)}${node}\n`)
    var r = f(node)
    depth--
    return r
  }
  else throw 'Unknown node type ' + node.type
}

function run(prog) {
  var r = interp(prog)
  console.log(`=> ${r}\n\n`)
  return r.toString()
}

var nodes = {
  c({e}) {
    return c(e)
  },
  
  v({x}) {
    return v(x)
    // or
    // throw 'Unknown variable ' + x
  },
  
  fun({x, e}) {
    return fun(x, e)
  },
  
  app({e1, e2}) {
    var f = interp(e1)
    var x = interp(e2)
    
    if (f.type !== 'fun') return 'Not a function ' + f
    return interp(subst(f.e, f.x, x))
  },
  
  subst({e1, x, e2}) {
    if (e1.type === 'c') {
      return e1
    }
    
    else if (e1.type === 'v') {
      if (e1.x === x) return e2
      else return e1 
    }
    
    else if (e1.type === 'fun') {
      if (e1.x === x) return e1 // Shadowing
      else return fun(e1.x, subst(e1.e, x, e2))
    }
    
    else if (e1.type === 'app') {
      return interp(app(subst(e1.e1, x, e2), subst(e1.e2, x, e2)))
    }
    
    else if (e1.type === 'subst') {
      if (e1.x === x) {
        throw 'HAHA'
      }
      else {
        var t = interp(e1)
        return interp(subst(t, x, e2))
      }
    }
    
    else
      return 'Non-exhaustive subst: ' + e1.type
  },
  
  R({e1, x, e2}) {
    console.log(e1, e2)
    var s = interp(e1)
    var y = interp(e2)
    
    if (s.type === 'fun') {
      return interp(fun(s.x, R(s.e, x, e2)))
    }
    
    if (s.type === 'subst') {
      if (x === s.x) {
        return interp(subst(s.e1, s.x, y))
      }      
      else return "Don't know watodo -_O_-"
    }
    
    else
      return 'Non-exhaustive R: ' + s.type
  },
}

// run(app(fun('x', v('x')), c(42))) //: 42

// run(app(fun('x', fun('y', v('x'))), c(42))) //: "(\y. (x [x=42]))"
// run(app(app(fun('x', fun('y', v('x'))), c(42)), c(0))) //: 42
// run(app(app(fun('x', fun('y', v('y'))), c(42)), c(0))) //: 0

run(app(R(app(fun('x', fun('y', v('x'))), c(42)), 'x', c(2)), c(0))) //: "Non-exhaustive subst: R"

//run(subst(c(0), 'x', c(1))) //: "Unknown node type undefined"
//run(subst(v('x'), 'x', c(1))) //: "Unknown node type undefined"
//run(subst(v('y'), 'x', c(1))) //: "Unknown node type undefined"
// run(subst(fun('x', c(0)), 'x', c(1))) //: "Unknown node type undefined"
// run(subst(fun('y', c(0)), 'x', c(1))) //: "Unknown node type undefined"
