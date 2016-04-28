function f() {
  var a = new Array(100000)
  for (var i=0; i < a.length; ++i) {
    a[i] = i
  }
  return a[a.length-1]
}

function g() {
  var a = 0
  for (var i=0; i < 10000; ++i) {
    a += f()
  }
  return a
}

console.log(g())
