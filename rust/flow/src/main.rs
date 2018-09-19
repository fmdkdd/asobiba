use std::collections::HashMap;
use std::sync::atomic::{self, AtomicUsize};

// ~~~~~~~~~
// * Syntax
// ~~~~~~~~~

#[derive(Debug, Clone)]
enum Const {
  Number(i32),
  String(String),
  Undef,
}

#[derive(Debug, Copy, Clone)]
enum Primitive {
  Number,
  String,
  Undefined,
}

#[derive(Debug, Clone)]
enum Pred {
  Typeof(Primitive),
  Eq(Const),
  Truthy,
  Field(Field, Const),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Var(String);

#[derive(Debug, Clone)]
struct Field(String);

#[derive(Debug, Clone)]
enum Expr {
  Read(Var),
  Const(Const),
  Assign(Var, Box<Expr>),
  Func(Var, Box<Statement>, Box<Expr>),
  Call(Box<Expr>, Box<Expr>),
  Rec(Vec<(Field, Expr)>),
  Get(Box<Expr>, Field),
  Set(Box<Expr>, Field, Box<Expr>),
  Pred(Pred, Var),
  And(Box<Expr>, Box<Expr>),
  Or(Box<Expr>, Box<Expr>),
  Not(Box<Expr>),
}

#[derive(Debug, Clone)]
enum Statement {
  Expr(Expr),
  Var(Var, Expr),
  If(Expr, Box<Statement>, Box<Statement>),
  Seq(Box<Statement>, Box<Statement>),
  Skip,
}

// ~~~~~~~~~~~~~~~~~~
// ** Syntax helpers
// ~~~~~~~~~~~~~~~~~~

fn var(x: &str, e: Expr) -> Statement {
  let v = Var(x.to_string());
  Statement::Var(v.clone(), Expr::Assign(v, Box::new(e)))
}

fn val(x: &str) -> Expr {
  Expr::Read(Var(x.to_string()))
}

fn str(s: &str) -> Expr {
  Expr::Const(Const::String(s.to_string()))
}

fn num(n: i32) -> Expr {
  Expr::Const(Const::Number(n))
}

fn assign(x: &str, e: Expr) -> Expr {
  Expr::Assign(Var(x.to_string()), Box::new(e))
}

fn st(e: Expr) -> Statement {
  Statement::Expr(e)
}

fn rec(f: Vec<(&str, Expr)>) -> Expr {
  Expr::Rec(f.into_iter().map(|(s, e)| (Field(s.to_string()), e)).collect())
}

fn seq(mut p: Vec<Statement>) -> Statement {
  match p.len() {
    0 => Statement::Skip,
    1 => p.pop().unwrap(),
    _ => {
      let hd = p.remove(0);
      Statement::Seq(Box::new(hd), Box::new(seq(p)))
    }
  }
}

fn func(mut args: Vec<&str>, s: Statement, e: Expr) -> Expr {
  match args.len() {
    0 => unimplemented!(),
    1 => Expr::Func(Var(args.pop().unwrap().to_string()), Box::new(s), Box::new(e)),
    _ => {
      let hd = args.remove(0);
      Expr::Func(Var(hd.to_string()), Box::new(skip), Box::new(func(args, s, e)))
    }
  }

}

const skip : Statement = Statement::Skip;

// ~~~~~~~~~~~~~~~~~~~~~~~~
// * Constraint generation
// ~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, Clone)]
struct TypeVar(String);

static VAR_COUNTER: AtomicUsize = atomic::ATOMIC_USIZE_INIT;

fn fresh_typevar() -> TypeVar {
  let i = VAR_COUNTER.fetch_add(1, atomic::Ordering::SeqCst);
  TypeVar(format!("a{}", i))
}

#[derive(Debug, Clone)]
enum Type {
  Number,
  String(String),
  Void,
  Arrow(TypeVar, Effect, Box<Type>),
  Rec(Vec<(Field, TypeVar)>),
  Union(Box<Type>, Box<Type>),
  Var(TypeVar),
}

type Env = HashMap<Var, (Type, TypeVar)>;

#[derive(Debug, Clone)]
struct EffectVar(String);

#[derive(Debug, Clone)]
enum Effect {
  Bottom,
  Var(Var),
  EffectVar(EffectVar),
  Union(Box<Effect>, Box<Effect>),
}

fn effect_u(e1: Effect, e2: Effect) -> Effect {
  if let Effect::Bottom = e1 {
    e2
  } else if let Effect::Bottom = e2 {
    e1
  } else {
    Effect::Union(Box::new(e1), Box::new(e2))
  }
}

#[derive(Debug, Clone)]
enum TypeUse {
  Var(TypeVar),
  Call(Type, EffectVar, TypeVar),
  Get(Field, TypeVar),
  Set(Field, Type),
  Pred(Pred, TypeVar),
}

#[derive(Debug, Clone)]
enum EffectUse {
  Var(EffectVar),
  Havoc(Env),
}

#[derive(Debug, Clone)]
enum Constraint {
  T(Type, TypeUse),
  E(Effect, EffectUse),
}

fn constraint_u(c1: Vec<Constraint>, c2: Vec<Constraint>) -> Vec<Constraint> {
  let mut c3 = c1.clone();
  c3.append(&mut c2.clone());
  c3
}

#[derive(Debug, Clone)]
enum PredMap {
  Empty,
  Bind(Var, Pred),
  And(Box<PredMap>, Box<PredMap>),
  Or(Box<PredMap>, Box<PredMap>),
  Not(Box<PredMap>, Box<PredMap>),
  Exclude(Box<PredMap>, Effect),
}

fn pm_exclude(p: PredMap, e: Effect) -> PredMap {
  PredMap::Exclude(Box::new(p), e)
}

fn cgen_statement(env: Env, s: Statement) -> (Effect, Env, Vec<Constraint>) {
  match s {
    Statement::Expr(e) => {
      let (_, eff, _, env2, c) = cgen_expr(env, e);
      (eff, env2, c)
    }

    Statement::Var(x, e) => {
      // This is not in the paper, but we need to add x to the environment
      // before CG-Assign.
      let mut env2 = env.clone();
      env2.insert(x, (Type::Void, fresh_typevar()));
      let (_, eff, _, env3, c) = cgen_expr(env2, e);
      (eff, env3, c)
    }

    Statement::Seq(s1, s2) => {
      let (e1, env1, c1) = cgen_statement(env, *s1);
      let (e2, env2, c2) = cgen_statement(env1, *s2);
      (effect_u(e1, e2), env2, constraint_u(c1, c2))
    }

    _ => unimplemented!(),
  }
}

fn cgen_expr(env: Env, e: Expr) -> (Type, Effect, PredMap, Env, Vec<Constraint>) {
  match e {
    Expr::Const(c) => {
      let t = match c {
        Const::Number(_) => Type::Number,
        Const::String(s) => Type::String(s),
        Const::Undef     => Type::Void,
      };
      (t, Effect::Bottom, PredMap::Empty, env, vec![])
    }

    Expr::Read(x) => {
      match env.get(&x) {
        Some((t, _)) =>  (t.clone(), Effect::Bottom, PredMap::Bind(x, Pred::Truthy), env.clone(), vec![]),
        None    => panic!("Variable {:?} not found in env", x),
      }
    }

    Expr::Assign(x, e) => {
      let (t, eff, p, env2, c) = cgen_expr(env, *e);
      let (_, a) = env2.get(&x).unwrap();
      let mut env3 = env2.clone();
      env3.insert(x.clone(), (t.clone(), a.clone()));
      let mut c2 = c.clone();
      c2.push(Constraint::T(t.clone(), TypeUse::Var(a.clone())));
      (t, effect_u(eff, Effect::Var(x.clone())), pm_exclude(p, Effect::Var(x.clone())), env3, c2)
    }

    _ => unimplemented!(),
  }
}

fn cgen(s: Statement) -> (Effect, Env, Vec<Constraint>) {
  cgen_statement(HashMap::new(), s)
}

// ~~~~~~~
// * Main
// ~~~~~~~
fn main() {

  let p0 = st(str("nil"));

  let p1 = var("nil", str("nol"));

  let p2 = seq(vec![
    var("nil", str("nol")),
    st(assign("nil", num(0))),
  ]);

  let p3 =
    seq(vec![
      var("nil", rec(vec![("kind", str("nil"))])),
      var("cons", func(vec!["head", "tail"], skip, rec(vec![("kind", str("cons")),
                                                            ("head", val("head")),
                                                            ("tail", val("tail"))]))),
    ]);

  let p = p2;

  println!("{:?}", p);
  println!("{:?}", cgen(p));
}
