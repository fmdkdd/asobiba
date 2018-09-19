use std::collections::{HashMap, HashSet};
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

// Useful for manually constructing ASTs

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

// From Section 3.1

#[derive(Debug, Clone)]
struct TypeVar(String);

// A global shared counter using atomics
static VAR_COUNTER: AtomicUsize = atomic::ATOMIC_USIZE_INIT;

fn uid() -> usize {
  VAR_COUNTER.fetch_add(1, atomic::Ordering::SeqCst)
}

fn fresh_typevar() -> TypeVar {
  TypeVar(format!("a{}", uid()))
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

#[derive(Debug, Clone)]
struct Env {
  env: HashMap<Var, (Type, TypeVar)>,
}

impl Env {
  fn new() -> Env {
    Env {
      env: HashMap::new()
    }
  }

  fn add(&self, x: Var, t: (Type, TypeVar)) -> Env {
    let mut r = self.clone();
    r.env.insert(x, t);
    r
  }

  fn lookup(&self, x: &Var) -> (&Type, &TypeVar) {
    match self.env.get(&x) {
      Some((t, a)) => (t, a),
      None         => panic!("Variable {:?} not found in env", x),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct EffectVar(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Effect {
  Var(Var),
  EffectVar(EffectVar),
}

#[derive(Debug, Clone)]
struct EffectSet {
  effects: HashSet<Effect>,
}

impl EffectSet {
  fn new() -> Self {
    EffectSet {
      effects: HashSet::new(),
    }
  }

  fn add(&self, e: Effect) -> Self {
    let mut r = self.clone();
    r.effects.insert(e);
    r
  }

  fn union(&self, e: EffectSet) -> Self {
    let mut r = EffectSet::new();
    for e in self.effects.union(&e.effects) {
      r.effects.insert(e.clone());
    }
    r
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

#[derive(Debug, Clone)]
struct ConstraintSet {
  constraints: Vec<Constraint>,
}

impl ConstraintSet {
  fn new() -> Self {
    ConstraintSet {
      constraints: Vec::new(),
    }
  }

  fn add(&self, c: Constraint) -> Self {
    let mut r = self.clone();
    r.constraints.push(c);
    r
  }

  fn union(&self, c: ConstraintSet) -> Self {
    let mut r = self.clone();
    r.constraints.append(&mut c.constraints.clone());
    r
  }
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

fn cgen_statement(env: Env, s: Statement) -> (EffectSet, Env, ConstraintSet) {
  match s {
    Statement::Expr(e) => {
      let (_, eff, _, env2, c) = cgen_expr(env, e);
      (eff, env2, c)
    }

    Statement::Var(x, e) => {
      // This is not in the paper, but we need to add x to the environment
      // before CG-Assign.
      let env2 = env.add(x, (Type::Void, fresh_typevar()));
      let (_, eff, _, env3, c) = cgen_expr(env2, e);
      (eff, env3, c)
    }

    Statement::Seq(s1, s2) => {
      let (e1, env1, c1) = cgen_statement(env, *s1);
      let (e2, env2, c2) = cgen_statement(env1, *s2);
      (e1.union(e2), env2, c1.union(c2))
    }

    _ => unimplemented!(),
  }
}

fn cgen_expr(env: Env, e: Expr) -> (Type, EffectSet, PredMap, Env, ConstraintSet) {
  match e {
    Expr::Const(c) => {
      let t = match c {
        Const::Number(_) => Type::Number,
        Const::String(s) => Type::String(s),
        Const::Undef     => Type::Void,
      };
      (t, EffectSet::new(), PredMap::Empty, env, ConstraintSet::new())
    }

    Expr::Read(x) => {
      let (t, _) = env.lookup(&x);
      (t.clone(), EffectSet::new(), PredMap::Bind(x, Pred::Truthy), env.clone(), ConstraintSet::new())
    }

    Expr::Assign(x, e) => {
      let (t, eff, p, env2, c) = cgen_expr(env, *e);
      let (_, a) = env2.lookup(&x);
      let env3 = env2.add(x.clone(), (t.clone(), a.clone()));
      let c2 = c.add(Constraint::T(t.clone(), TypeUse::Var(a.clone())));
      (t, eff.add(Effect::Var(x.clone())), pm_exclude(p, Effect::Var(x.clone())), env3, c2)
    }

    _ => unimplemented!(),
  }
}

fn cgen(s: Statement) -> (EffectSet, Env, ConstraintSet) {
  cgen_statement(Env::new(), s)
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
