|| klet id = 0
function fresh() {
  return 'a' + (id++)
}

let expr_to_types = new Map()

function typ(e) {
  if (!expr_to_types.has(e)) {
    expr_to_types.set(e, fresh())
  }

  return expr_to_types.get(e)
}

function arrow(a, b) {
  return {
    type: '->',
    a, b,
  }
}

function defun(name, arg, body) {
  return {
    type: 'FUN',
    name, arg, body,

    typ_constr(env) {
      let a = fresh()
      let b = fresh()

      env.push([typ(arg), a])
      env.push([typ(body), b])
      env.push([typ(name), arrow(a, b)])

      body.typ_constr(env)
    }
  }
}

function num(n) {
  return {
    type: 'NUM',
    n,

    typ_constr(env) {
      env.push([typ(this), 'num'])
    }
  }
}

function name(x) {
  return {
    type: 'NAME',
    x,

    typ_constr(env) {
      env.push([typ(this), typ(x)])
    }
  }
}

function apply(f, x) {
  return {
    type: 'APPLY',
    f, x,

    typ_constr(env) {
      let a = fresh()
      let b = fresh()
      env.push([typ(f), arrow(a, b)])
      env.push([typ(x), a])
      env.push([typ(this), b])

      f.typ_constr(env)
      x.typ_constr(env)
    }
  }
}

function iff(cond, th, el) {
  return {
    type: 'IF',
    cond, th, el,

    typ_constr(env) {
      let a = fresh()
      env.push([typ(cond), 'boolean'])
      env.push([typ(th), a])
      env.push([typ(el), a])
      env.push([typ(this), a])

      cond.typ_constr(env)
      th.typ_constr(env)
      el.typ_constr(env)
    }
  }
}

let p0 = defun('length', 'x', iff(apply(name('null'), name('x')), num(0),
                                  apply(name('plus'), apply(name('length'),
                                                            apply(name('tl'), name('x'))), num(1))))

let env = []
p0.typ_constr(env) //: undefined
pp(env)

function pp(env) {
  for (let [k, v] of expr_to_types.entries()) {
    console.log(`${v} -- `, k.type || k)
  }
  console.log(env.map(pp_typ).join('\n'))
}

function pp_typ(t) {
  if (t[1].type === '->') {
    return `${t[0]} = ${t[1].a} -> ${t[1].b}`
  } else {
    return `${t[0]} = ${t[1]}`
  }
}

function pp_nod(n) {

}
