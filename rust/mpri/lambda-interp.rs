// Lambda-calculus interpreter with environments, closures and de Bruijn indices

#[derive(PartialEq, Eq, Clone, Debug)]
enum Term<'a> {
  Const(usize),
  Var(usize),
  Lambda(&'a Term<'a>),
  Apply(&'a Term<'a>, &'a Term<'a>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Value<'a> {
  Const(usize),
  Closure(Term<'a>, Environment<'a>)
}

type Environment<'a> = Vec<Value<'a>>;

fn eval_with_env<'a>(t: &Term<'a>, e: &Environment<'a>) -> Value<'a> {
  match *t {
    Term::Const(n)    => Value::Const(n),
    Term::Var(n)      => e[e.len() - 1 - n].clone(),
    Term::Lambda(c)   => Value::Closure(c.clone(), e.clone()),
    Term::Apply(a, b) => {
      let va = eval_with_env(a, &e);
      let vb = eval_with_env(b, &e);
      match va {
        Value::Closure(c, ee) => {
          let mut e2 = ee.clone();
          e2.push(vb);
          eval_with_env(&c, &e2)
        },
        _ => panic!("Cannot apply a non-closure"),
      }
    },
  }
}

fn eval<'a>(t: &Term<'a>) -> Value<'a> {
  eval_with_env(t, &Vec::new())
}

#[test]
fn expressions() {
  use Term::*;
  assert_eq!(eval(&Const(42)), Value::Const(42));
  assert_eq!(eval(&Lambda(&Const(42))), Value::Closure(Const(42), vec![]));
  assert_eq!(eval(&Apply(&Lambda(&Var(0)),
                         &Const(42))),
             Value::Const(42));
}

fn main() {
  use Term::*;
  println!("{:?}", eval(&Apply(&Lambda(&Apply(&Lambda(&Apply(&Var(0), &Var(1))),
                                              &Var(0))),
                               &Lambda(&Var(0)))));
}
