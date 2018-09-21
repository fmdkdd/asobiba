use std::collections::{HashMap, HashSet};
use std::fmt;
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
  Not(Box<Pred>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Var(String);

impl fmt::Debug for Var {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self.0)
  }
}

#[derive(Clone)]
struct Field(String);

impl fmt::Debug for Field {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self.0)
  }
}

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

fn locals(s: &Statement) -> Vec<Var> {
  use Statement::*;

  let mut v = Vec::new();

  if let Var(x, _) = s {
    v.push(x.clone())
  }
  v
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

fn call(f: &str, args: Vec<Expr>) -> Expr {
  call1(Expr::Read(Var(f.to_string())), args)
}

fn call1(e: Expr, mut args: Vec<Expr>) -> Expr {
  match args.len() {
    0 => unimplemented!(),
    1 => Expr::Call(Box::new(e),
                    Box::new(args.remove(0))),
    _ => {
      let hd = args.remove(0);
      let c = Expr::Call(Box::new(e), Box::new(hd));
      call1(c, args)
    }
  }
}

const skip : Statement = Statement::Skip;

// ~~~~~~~~~~~~~~~~~~~~~~~~
// * Constraint generation
// ~~~~~~~~~~~~~~~~~~~~~~~~

// From Section 3.1

#[derive(Clone, PartialEq, Eq)]
struct TypeVar(String);

impl fmt::Debug for TypeVar {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self.0)
  }
}

// A global shared counter using atomics
static VAR_COUNTER: AtomicUsize = atomic::ATOMIC_USIZE_INIT;

fn uid() -> usize {
  VAR_COUNTER.fetch_add(1, atomic::Ordering::SeqCst)
}

fn fresh_typevar(prefix: &str) -> TypeVar {
  TypeVar(format!("{}{}", prefix, uid()))
}

fn fresh_effectvar(prefix: &str) -> EffectVar {
  EffectVar(format!("{}{}", prefix, uid()))
}

#[derive(Debug, Clone)]
enum Type {
  Number,
  String(String),
  Void,
  Arrow(TypeVar, EffectSet, Box<Type>),
  Rec(Vec<(Field, TypeVar)>),
  Union(Box<Type>, Box<Type>),
  Var(TypeVar),
}

#[derive(Clone)]
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

  fn update(&self, x: Var, t: (Type, TypeVar)) -> Env {
    self.add(x, t)
  }

  fn lookup(&self, x: &Var) -> (&Type, &TypeVar) {
    match self.env.get(&x) {
      Some((t, a)) => (t, a),
      None         => panic!("Variable {:?} not found in env", x),
    }
  }

  fn join(&self, e: &Env) -> Env {
    let mut r = self.clone();
    for (x, (t, a)) in &e.env {
      match self.env.get(&x) {
        None                      => r.env.insert(x.clone(), (t.clone(), a.clone())),
        Some((t_, a_)) if a_ == a => r.env.insert(x.clone(), (Type::Union(Box::new(t.clone()),
                                                                          Box::new(t_.clone())), a.clone())),
        Some((_, a_)) => panic!("Same variable {:?}, different type var {:?} and {:?}",
                                x, a, a_)
      };
    }
    r
  }

  fn erase(&self) -> Env {
    let mut r = Env::new();
    for (x, (_, a)) in self.env.iter() {
      r.env.insert(x.clone(), (Type::Var(a.clone()), a.clone()));
    }
    r
  }

  fn widen(&self) -> (Env, ConstraintSet) {
    let r = self.clone();
    let c = ConstraintSet::new();
    for (x, (t, a)) in self.env.iter() {
      let b = fresh_typevar("β");
      r.update(x.clone(), (Type::Var(b.clone()), a.clone()));
      c.add(Constraint::T(t.clone(), TypeUse::Var(b.clone())));
      c.add(Constraint::T(Type::Var(b), TypeUse::Var(a.clone())));
    }
    (r, c)
  }

  fn refine(&self, psi: &PredMap) -> (Env, ConstraintSet) {
    use PredMap::*;

    match psi {
      Empty => (self.clone(), ConstraintSet::new()),
      Bind(x, p) => {
        let (t, a) = self.lookup(x);
        let b = fresh_typevar("β");
        let env2 = self.update(x.clone(), (Type::Var(b.clone()), a.clone()));
        let c = ConstraintSet::new();
        (env2, c.add(Constraint::T(t.clone(), TypeUse::Pred(p.clone(), b))))
      }
      And(p1, p2) => {
        let (env1, c1) = self.refine(p1);
        let (env2, c2) = env1.refine(p2);
        (env2, c1.union(&c2))
      }
      Or(p1, p2) => {
        let (env1, c1) = self.refine(p1);
        let (env2, c2) = env1.refine(p2);
        (env1.join(&env2), c1.union(&c2))
      }
      Exclude(p, eff) => {
        let (env1, c1) = self.refine(p);
        let (env2, c2) = env1.widen();
        let mut env3 = HavocEnv::new();
        for (x, (t, a)) in self.env.iter() {
          let (b, a_) = env2.lookup(x);
          if a == a_ {
            env3.env.insert(x.clone(), (b.clone(), t.clone()));
          } else {
            unreachable!();
          }
        }
        (env2, c1.union(&c2).add(Constraint::E(eff.clone(), EffectUse::Havoc(env3))))
      }
      Not(p) => {
        match *p.clone() {
          Empty          => self.refine(&Empty),
          Bind(x, p)     => self.refine(&Bind(x, Pred::Not(Box::new(p)))),
          And(p1, p2)    => self.refine(&And(Box::new(Not(p1)), Box::new(Not(p2)))),
          Or(p1, p2)     => self.refine(&Or(Box::new(Not(p1)), Box::new(Not(p2)))),
          Exclude(p1, e) => self.refine(&Exclude(Box::new(Not(p1)), e)),
          Not(p)         => self.refine(&p),
        }
      }
    }
  }
}

impl fmt::Debug for Env {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "Env {:?}", self.env)
  }
}

#[derive(Clone)]
struct HavocEnv {
  env: HashMap<Var, (Type, Type)>,
}

impl HavocEnv {
  fn new() -> Self {
    Self {
      env: HashMap::new(),
    }
  }

  fn from_env(e: &Env) -> Self {
    let mut r = HavocEnv::new();
    for (x, (t, a)) in e.env.iter() {
      r.env.insert(x.clone(), (t.clone(), Type::Var(a.clone())));
    }
    r
  }
}

impl fmt::Debug for HavocEnv {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "HavocEnv {:?}", self.env)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct EffectVar(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Effect {
  Var(Var),
  EffectVar(EffectVar),
}

#[derive(Clone)]
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

  fn union(&self, e: &EffectSet) -> Self {
    let mut r = EffectSet::new();
    for e in self.effects.union(&e.effects) {
      r.effects.insert(e.clone());
    }
    r
  }
}

impl fmt::Debug for EffectSet {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "EffectSet {:?}", self.effects)
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
  Havoc(HavocEnv),
}

#[derive(Debug, Clone)]
enum Constraint {
  T(Type, TypeUse),
  E(Effect, EffectUse),
}

#[derive(Clone)]
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

  fn union(&self, c: &ConstraintSet) -> Self {
    let mut r = self.clone();
    r.constraints.append(&mut c.constraints.clone());
    r
  }
}

impl fmt::Debug for ConstraintSet {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ConstraintSet {:?}", self.constraints)
  }
}

#[derive(Debug, Clone)]
enum PredMap {
  Empty,
  Bind(Var, Pred),
  And(Box<PredMap>, Box<PredMap>),
  Or(Box<PredMap>, Box<PredMap>),
  Not(Box<PredMap>),
  Exclude(Box<PredMap>, Effect),
}

fn pm_exclude(p: PredMap, e: Effect) -> PredMap {
  PredMap::Exclude(Box::new(p), e)
}

fn cgen_statement(env: &Env, s: &Statement) -> (EffectSet, Env, ConstraintSet) {
  match s {
    Statement::Expr(e) => {
      let (_, eff, _, env2, c) = cgen_expr(env, e);
      (eff, env2, c)
    }

    Statement::Var(x, e) => {
      // This is not in the paper, but we need to add x to the environment
      // before CG-Assign.
      let env2 = env.add(x.clone(), (Type::Void, fresh_typevar("α")));
      let (_, eff, _, env3, c) = cgen_expr(&env2, e);
      (eff, env3, c)
    }

    Statement::Seq(s1, s2) => {
      let (e1, env1, c1) = cgen_statement(env, s1);
      let (e2, env2, c2) = cgen_statement(&env1, s2);
      (e1.union(&e2), env2, c1.union(&c2))
    }

    Statement::If(e, s1, s2) => {
      let (_, eff, p, env_, c1) = cgen_expr(env, e);
      let (env1, c2) = env_.refine(&p);
      let (eff1, env1_, c3) = cgen_statement(&env1, s1);
      let (env2, c4) = env_.refine(&PredMap::Not(Box::new(p)));
      let (eff2, env2_, c5) = cgen_statement(&env2, s2);
      let env__ = env1_.join(&env2_);
      (eff.union(&eff1).union(&eff2), env__, c1.union(&c2).union(&c3).union(&c4).union(&c5))
    }

    Statement::Skip => {
      (EffectSet::new(), env.clone(), ConstraintSet::new())
    }
  }
}

fn cgen_expr(env: &Env, e: &Expr) -> (Type, EffectSet, PredMap, Env, ConstraintSet) {
  match e {
    Expr::Const(c) => {
      let t = match c {
        Const::Number(_) => Type::Number,
        Const::String(s) => Type::String(s.clone()),
        Const::Undef     => Type::Void,
      };
      (t, EffectSet::new(), PredMap::Empty, env.clone(), ConstraintSet::new())
    }

    Expr::Read(x) => {
      let (t, _) = env.lookup(&x);
      (t.clone(), EffectSet::new(), PredMap::Bind(x.clone(), Pred::Truthy), env.clone(), ConstraintSet::new())
    }

    Expr::Assign(x, e) => {
      let (t, eff, p, env2, c) = cgen_expr(env, e);
      let (_, a) = env2.lookup(&x);
      let env3 = env2.add(x.clone(), (t.clone(), a.clone()));
      let c2 = c.add(Constraint::T(t.clone(), TypeUse::Var(a.clone())));
      (t, eff.add(Effect::Var(x.clone())), pm_exclude(p, Effect::Var(x.clone())), env3, c2)
    }

    Expr::Func(x, s, e) => {
      let mut env1 = env.erase();
      let a = fresh_typevar("α");
      env1.env.insert(x.clone(), (Type::Var(a.clone()), a.clone()));
      let l = locals(s);
      for xi in l.iter() {
        env1.env.insert(xi.clone(), (Type::Void, fresh_typevar("α")));
      }

      let (eff_, env2, c_) = cgen_statement(&env1, s);
      let (t, eff, _, _, c) = cgen_expr(&env2, e);

      let mut eff2 = eff_.union(&eff);
      // Remove x, xi from eff2
      for xi in l {
        eff2.effects.remove(&Effect::Var(xi));
      }

      let arr = Type::Arrow(a, eff2, Box::new(t));

      (arr, EffectSet::new(), PredMap::Empty, env.clone(), c.union(&c_))
    }

    Expr::Call(e1, e2) => {
      let (t1, eff1, _, env1, c1) = cgen_expr(env, e1);
      let (t2, eff2, _, env2, c2) = cgen_expr(&env1, e2);
      let a = fresh_typevar("α");
      let w = fresh_effectvar("ω");
      let (env3, c3) = env2.widen();
      let eff = eff1.union(&eff2).add(Effect::EffectVar(w.clone()));
      let c = c1.union(&c2).union(&c3)
        .add(Constraint::E(Effect::EffectVar(w.clone()), EffectUse::Havoc(HavocEnv::from_env(&env3))))
        .add(Constraint::T(t1, TypeUse::Call(t2, w, a.clone())));
      (Type::Var(a), eff, PredMap::Empty, env3, c)
    }

    Expr::Rec(bs) => {
      let mut g = env.clone();
      let mut c = ConstraintSet::new();
      let mut eff = EffectSet::new();
      let mut r = vec![];
      for (f, e) in bs {
        let (t, eff_, _, env_, c_) = cgen_expr(&g, e);
        let a = fresh_typevar("α");
        r.push((f.clone(), a.clone()));
        eff = eff.union(&eff_);
        g = env_;
        c = c.union(&c_).add(Constraint::T(t, TypeUse::Var(a)));
      }

      r.reverse();
      (Type::Rec(r), eff, PredMap::Empty, g, c)
    }

    Expr::Get(e, f) => {
      let (t, eff, _, env_, c) = cgen_expr(&env, e);
      let a = fresh_typevar("α");
      (Type::Var(a.clone()), eff, PredMap::Empty, env_,
       c.add(Constraint::T(t, TypeUse::Get(f.clone(), a))))
    }

    Expr::Set(e1, f, e2) => {
      let (t1, eff1, _, env1, c1) = cgen_expr(&env, e1);
      let (t2, eff2, _, env2, c2) = cgen_expr(&env1, e2);
      (t2.clone(), eff1.union(&eff2), PredMap::Empty, env2,
       c1.union(&c2).add(Constraint::T(t1, TypeUse::Set(f.clone(), t2))))
    }

    _ => unimplemented!()

  }
}

fn cgen(s: &Statement) -> (EffectSet, Env, ConstraintSet) {
  let mut env = Env::new();

  let a = fresh_typevar("α");
  let r = Type::Arrow(a.clone(),
                      EffectSet::new().add(Effect::EffectVar(fresh_effectvar("ω"))),
                      Box::new(Type::Var(a.clone())));

  env = env.add(Var("+".to_string()), (Type::Arrow(a.clone(),
                                                   EffectSet::new().add(Effect::EffectVar(fresh_effectvar("ω"))),
                                                   Box::new(r)), a));

  cgen_statement(&env, s)
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

  let p4 = st(call("+", vec![num(0), num(1)]));

  // let p4 =
  //   seq(vec![
  //     var("nil", rec(vec![("kind", str("nil"))])),
  //     var("cons", func(vec!["head", "tail"], skip, rec(vec![("kind", str("cons")),
  //                                                           ("head", val("head")),
  //                                                           ("tail", val("tail"))]))),
  //     var("sum", func(vec!["list"], seq(vec![
  //       var("ret", num(0)),
  //       iff(eq(get("list", "kind"), str("cons")),
  //           get("list", "head"),
  //           skip)
  //     ]),
  //                     read("ret"))

  //   ]);

  let p = p4;

  println!("{:?}", p);
  println!("{:#?}", cgen(&p));
}
