// Lambda-calculus interpreter with
// environments, closures and de Bruijn indices

#[derive(Clone)]
enum Term {
  Const(usize),
  Var(usize),
  Lambda(Box<Term>),
  Apply(Box<Term>, Box<Term>),
}

#[derive(Clone)]
enum Value {
  Const(usize),
  Closure(Box<Term>, Environment)
}

type Environment = Vec<Value>;

fn eval(t: &Term, e: &Environment) -> Value {
  match t {
    &Term::Const(n)  => Value::Const(n),
    &Term::Var(n)    => e[e.len() - 1 - n].clone(),
    &Term::Lambda(ref t) => Value::Closure(t.clone(), e.clone()),
    &Term::Apply(ref a, ref b) => {
      let va = eval(&a, e);
      let vb = eval(&b, e);
      match va {
        Value::Closure(c, ee) => {
          let mut e2 = ee.clone();
          e2.push(vb);
          eval(&c, &e2)
        },
        _ => panic!("Not a closure"),
      }
    },
  }
}

fn print(v: &Value) -> String {
  match v {
    &Value::Const(n) => format!("{}", n),
    &Value::Closure(ref c, ref e) => "closure".into(),
  }
}

fn main() {
  use Term::*;

  println!("{}", print(&eval(&Apply(Box::new(Lambda(Box::new(Var(0)))),
                                    Box::new(Const(42))),
                             &Vec::new())
  ));

  // println!("{}", print(&eval(&Lambda(Box::new(Apply(Box::new(Lambda(Box::new(Apply(Box::new(Var(0)),
  //                                                                              Box::new(Var(1)))))),
  //                                                 Box::new(Var(0))))),
  //                            &Vec::new())));
}
