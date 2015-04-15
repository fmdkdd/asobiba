/* eslint-disable */

// Relational programming rather than functional.
// μ as in micro-kernel.  Small is simpler.

var fresh_counter = 0

// Wrapper for variables
function Var(name) {this.name = name}
var is_var = x => x instanceof Var
var eq_var = (a, b) => a.name === b.name

// Walk searches for a variable's value in the substitution.
var walk = (u, subst) =>
  is_var(u) && u.name in subst ? walk(subst[u.name], subst)
                               : u

// ext_s extends the substitution with a new (var, val) couple.
var ext_s = (lvar, val, subst) => ({
  __proto__: subst,
  [lvar.name]: val
})

// Pretty printer for substitutions and variables
var pp = subst => {
  var o = Object.create(null)
  for (var k in subst)
    o[k] = is_var(subst[k]) ? `#${subst[k].name}` : subst[k]
  return o
}

var a = new Var('a')
var b = new Var('b')
var c = new Var('c')

walk(a, ext_s(a, b, ext_s(b, 1))) //: 1
walk(a, ext_s(b, 1, ext_s(a, b))) //: 1

// The core of the kernel : unify
var unify = (u, v, subst) => {
  u = walk(u, subst)
  v = walk(v, subst)
  if (is_var(u) && is_var(v) && eq_var(u, v)) return subst
  else if (is_var(u)) return ext_s(u, v, subst)
  else if (is_var(v)) return ext_s(v, u, subst)
  else if (Array.isArray(u) && Array.isArray(v)) {
    if (u.length === 0 && v.length === 0) return subst // avoid infinite recursion
    var [[u1, ...us], [v1, ...vs]] = [u, v]
    var s = unify(u1, v1, subst)
    return s && unify(us, vs, s)
  }
  else if (u === v) return subst
  else return false
}

pp(unify(a, b, {})) //: Object {a:"#b"}
pp(unify(a, 1, unify(a, b, {}))) //: Object {b:1,a:"#b"}
pp(unify([a,b], [1,2], {})) //: Object {b:2,a:1}

// Finite depth-first search
// var mplus = (s1, s2) => s1.concat(s2)
// var bind = (s, g) => s.length === 0 ? mzero()
//                                     : mplus(g(s[0]), bind(s.slice(1), g))

// Infinite streams
var mplus = (s1, s2) =>
  typeof s1 === 'function' ? _ => mplus(s2, s1())
                           : s1.concat(s2)

var bind = (s, g) => {
  if (s.length === 0) return mzero()
  else if (typeof s === 'function') return _ => bind(s(), g)
  else return mplus(g(s[0]), bind(s.slice(1), g))
}

// Goal constructors
var unit = x => [x]
var mzero = _ => []

var eq = (u, v) => subst => {
  var s = unify(u, v, subst)
  return s ? unit(s) : mzero()
}

var call_fresh = f => f(new Var(fresh_counter++))

var disj = (g1, g2) => subst =>
  mplus(g1(subst), g2(subst))

var conj = (g1, g2) => subst =>
  bind(g1(subst), g2)

var force = f => typeof f === 'function' ? force(f()) : f

var run = g => g(Object.create(null)).map(s => pp(force(s)))

run(eq(1, 1)) //: [Object {}]
run(eq(1, 2)) //: []
run(eq(a, 3)) //: [Object {a:3}]
run(eq([a, 2], [1,2])) //: [Object {a:1}]

var choice = (lvar, [h, ...t]) =>
  h == null ? mzero
            : disj(eq(lvar, h),
                   choice(lvar, t))

run(choice(a, [1,2,3])) //: [Object {a:1},Object {a:2},Object {a:3}]

var common_el = (l1, l2) =>
  call_fresh(a => conj(choice(a, l1), choice(a, l2)))

run(common_el([1,2,3], [2,3,4,5])) //: [Object {0:2},Object {0:3}]

var conso = (a, b, list) => eq([a, b], list)

run(conso(1, [2,3], a)) //: [Object {a:[1,[2,3]]}]
run(conso(a, [2,3], [1,[2,3]])) //: [Object {a:1}]
run(conso(a, b, [1,[2,3]])) //: [Object {b:[2,3],a:1}]
run(conso(a, b, [1,c])) //: [Object {b:"#c",a:1}]




var fives = x => disj(eq(x, 5),
                      (subst) => _ => fives(x)(subst))

run(fives(a)) //: [Object {a:5},Object {0:Object {a:5},1:function}]

var minus = (n, m, n_m) => eq(n_m, n-m)

run(minus(10, 5, 5)) //: [Object {}]
run(minus(10, 5, a)) //: [Object {a:5}]

var fact = (n, f) => call_fresh(f1 => call_fresh(n1 =>
  disj(conj(eq(n, 0), eq(f, 1)),
       conj(subst => _ => fact(n1, f1)(subst), conj(eq(f, n * f1), minus(n, 1, n1))))))

run(fact(0, a)) //: [Object {a:1},Object {0:Object {2:-1}}]
run(fact(1, a)) //: [Object {0:Object {a:NaN},1:Object {6:0}}]
