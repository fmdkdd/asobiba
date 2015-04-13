/* Traduction of http://www.pobox.com/~oleg/ftp/Scheme/sokuza-kanren.scm
   to JavaScript */

var fail = x => []
var succeed = x => [x]

var disj = (f1, f2) => (x => f1(x).concat(f2(x)))
var conj = (f1, f2) => (x => [].concat.apply([], f1(x).map(f2)))

disj(x => succeed(x + 1), x => succeed(x + 10))(100) //: [101,110]

conj(disj(x => succeed(x + 1), x => succeed(x + 10)),
     disj(succeed, succeed))(100) //: [101,101,110,110]


var lvar = name => name
var is_lvar = x => typeof x === 'string'

is_lvar(lvar('a')) //: true

var empty_subst = () => Object.create(null)
var ext_s = (lvar, value, s) => ({
  __proto__: s,
  [lvar]: value
})

var pp = (s) => {
  var o = Object.create(null)
  for (var k in s)
    o[k] = s[k]
  return o
}

var lookup = (lvar, s) => {
  if (is_lvar(lvar) && lvar in s) return lookup(s[lvar], s)
  else return lvar
}

lookup('a', ext_s('a', 'b', ext_s('b', 1, empty_subst()))) //: 1
lookup('a', ext_s('b', 1, ext_s('a', 'b', empty_subst()))) //: 1

var unify = (t1, t2, s) => {
  //console.log('unify', t1, t2, s)
  t1 = lookup(t1, s)
  t2 = lookup(t2, s)
  if (t1 === t2) return s
  else if (is_lvar(t1)) return ext_s(t1, t2, s)
  else if (is_lvar(t2)) return ext_s(t2, t1, s)
  else if (Array.isArray(t1) && Array.isArray(t2)) {
    if (t1.length === 0 && t2.length === 0) return s
    s = unify(t1[0], t2[0], s)
    return s && unify(t1.slice(1), t2.slice(1), s)
  }
  else false
}

var vx = lvar('x')
var vy = lvar('y')
var vz = lvar('z')
var vq = lvar('q')

pp(unify(vx, 1, unify(vx, vy, empty_subst()))) //: Object {y:1,x:"y"}

var eq = (t1, t2) => (s => {
  var k = unify(t1, t2, s)
  if (k) return succeed(k)
  else return fail(s)
})

var run = g => g(empty_subst()).map(pp)

run(eq(1, 1)) //: [Object {}]
run(eq([vx,2], [1,2])) //: [Object {x:1}]
run(eq(vx, [1,2])) //: [Object {x:[1,2]}]
run(eq([vx,vy], [1,2])) //: [Object {y:2,x:1}]
run(eq([vx,vy], [1,2,3])) //: []
run(conj(eq(vx, vy), eq(vx, 1))) //: [Object {y:1,x:"y"}]

var lt = (t1, t2) => (s => {

})

var choice = (lvar, lst) => {
  if (lst.length === 0) return fail
  else return disj(eq(lvar, lst[0]),
                   choice(lvar, lst.slice(1)))
}

run(choice(2, [1,2,3])) //: [Object {}]
run(choice(10, [1,2,3])) //: []
run(choice(vx, [1,2,3])) //: [Object {x:1},Object {x:2},Object {x:3}]

var common_el = (l1, l2) =>
  conj(choice(vx, l1), choice(vx, l2))

run(common_el([1,2,3], [3,4,1,5])) //: [Object {x:1},Object {x:3}]

var conso = (a, b, l) => eq([a,b], l)

run(conso(1, [2,3], vx)) //: [Object {x:[1,[2,3]]}]
run(conso(vx, vy, [1,[2,[3]]])) //: [Object {y:[2,[3]],x:1}]

var appendo = (l1, l2, l3) => {
  var h = lvar('h')
  var t = lvar('t')
  var l3p = lvar('l3p')
  return disj(conj(eq(l1, undefined),
                   eq(l2, l3)),
              conj(conso(h, t, l1),
                   (s => conj(conso(h, l3p, l3),
                              (s => appendo(t, l2, l3p)(s)))(s))))
}

run(appendo(vx, vy, [1])) //: [Object {y:[1],x:undefined},Object {y:undefined,t:undefined,l3p:undefined,h:1,x:["h","t"]}]
run(appendo(vx, vy, [1,2,[3,[4,[5]]]])) //: [Object {y:[1,2,[3,[4,[5]]]],x:undefined}]
