var c = (a) => ({type: 'c', a})
var plus = (a,b) => ({type: 'plus', a,b})
var mult = (a,b) => ({type: 'mult', a,b})

var explanation = (v,e) => ({v,e})

function ev(n) {
  if (n.type === 'plus') {
    var a = ev(n.a)
    var b = ev(n.b)
    return explanation(a.v + b.v, `${a.v} (${a.e}) + ${b.v} (${b.e})`)
  }
  else if (n.type === 'mult') {
    var a = ev(n.a)
    var b = ev(n.b)
    return explanation(a.v * b.v, `${a.v} (${a.e}) * ${b.v} (${b.e})`)
  }
  else if (n.type === 'c')
    return explanation(n.a, '.')
}

var p1 = plus(c(1), c(1))
var p2 = mult(c(3), c(1))

ev(p1) //: Object {v:2,e:"1 (.) + 1 (.)"}
ev(p2) //: Object {v:3,e:"3 (.) * 1 (.)"}

ev(plus(plus(c(1),c(1)),c(1))) //: Object {v:3,e:"2 (1 (.) + 1 (.)) + 1 (.)"}
