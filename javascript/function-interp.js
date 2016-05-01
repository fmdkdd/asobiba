/* eslint-disable */

/// An interpreter with an AST

// AST builders
function v(v) { return {type: 'var', v}}
function c(c) { return {type: 'const', c}}
function lam(param, body) { return {type: 'lambda', param, body}}
function call(lam, param) { return {type: 'call', lam, param}}

// Interpretation function
function exec(ast) {
  return exec_node(ast, {})
}

function exec_node(n, env) {
  switch (n.type) {
  case 'var':
    return env[n.v]

  case 'const':
    return n.c

  case 'lambda':
    return {body: n.body, param: n.param, env}

  case 'call':
    var l = exec_node(n.lam, env)
    var p = exec_node(n.param, env)

    var env2 = Object.create(l.env)
    env2[l.param] = p
    return exec_node(l.body, env2)
  }
}

exec(call(lam('a', v('a')), c(1))) //: 1


// An interpreter without an AST
var env = {}

function v(v) {
  return env[v]
}

function c(c) {
  return c
}

function lam(param, body) {
  return {body, param, env}
}

function call(lam, param) {
  var env2 = Object.create(lam.env)
  env2[lam.param] = param
  var envbak = env
  env = env2
  var ret = lam.body()
  env = envbak
  return ret
}


function exec(p) {
  return p
}

// Doesn't work:
exec(call(lam('a', v('a')), c(1))) //: TypeError: lam.body is not a function

// But this does:
exec(call(lam('a', () => v('a')), c(1))) //: 1

// But then it's not an AST any more.  The program cannot be eagerly evaluated
// because it contains code that could be executed at a later time, or never at
// all.  Using an AST is like using a thunk, it's a means to delay computation.
// The AST has the advantage of being a data structure, which can be serialized
// and used by other programs.
