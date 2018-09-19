use std::collections::HashMap;

// * Syntax

#[derive(Debug)]
enum Const {
  Number(i32),
  String(String),
  Undef,
}

#[derive(Debug)]
enum Primitive {
  Number,
  String,
  Undefined,
}

#[derive(Debug)]
enum Pred {
  Typeof(Primitive),
  Eq(Const),
  Truthy,
  Field(String, Const),
}

#[derive(Debug)]
enum Expr {
  Read(String),
  Const(Const),
  Assign(String, Box<Expr>),
  Func(String, Box<Statement>, Box<Expr>),
  Call(Box<Expr>, Box<Expr>),
  Rec(Vec<(String, Expr)>),
  Get(Box<Expr>, String),
  Set(Box<Expr>, String, Box<Expr>),
  Pred(Pred, String),
  And(Box<Expr>, Box<Expr>),
  Or(Box<Expr>, Box<Expr>),
  Not(Box<Expr>),
}

#[derive(Debug)]
enum Statement {
  Expr(Expr),
  Var(String, Expr),
  If(Expr, Box<Statement>, Box<Statement>),
  Seq(Box<Statement>, Box<Statement>),
  Skip,
}

// ** Syntax helpers

fn var(x: &str, e: Expr) -> Statement {
  Statement::Var(x.to_string(), e)
}

fn val(x: &str) -> Expr {
  Expr::Read(x.to_string())
}

fn str(s: &str) -> Expr {
  Expr::Const(Const::String(s.to_string()))
}

fn rec(f: Vec<(&str, Expr)>) -> Expr {
  Expr::Rec(f.into_iter().map(|(s, e)| (s.to_string(), e)).collect())
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
    1 => Expr::Func(args.pop().unwrap().to_string(), Box::new(s), Box::new(e)),
    _ => {
      let hd = args.remove(0);
      Expr::Func(hd.to_string(), Box::new(skip), Box::new(func(args, s, e)))
    }
  }

}

const skip : Statement = Statement::Skip;

// * Constraint generation

#[derive(Debug)]
enum Type {
  Number,
  String,
  Void,
  Arrow(String, Effect, Box<Type>),
  Rec(Vec<(String, String)>),
  Union(Box<Type>, Box<Type>),
  Var(String),                  // type variable
}

type Env = HashMap<String, Type>;

#[derive(Debug)]
enum Effect {
  Bottom,
  Sym(String),                  // variable symbol
  Var(String),                  // effect variable
  Union(Box<Effect>, Box<Effect>),
}

#[derive(Debug)]
enum TypeUse {
  Var(String),
  Call(Type, String, String),
  Get(String, String),
  Set(String, Type),
  Pred(Pred, String),
}

#[derive(Debug)]
enum EffectUse {
  Var(String),
  Havoc(Env),
}

#[derive(Debug)]
enum Constraint {
  T(Type, TypeUse),
  E(Effect, EffectUse),
}


fn cgen_statement(env: Env, s: Statement) -> (Effect, Env, Option<Constraint>) {
  match s {
    Statement::Expr(e) => {
      let (_, eff, _, env2, c) = cgen_expr(env, e);
      (eff, env2, c)
    }
    _ => unimplemented!(),
  }
}

fn cgen_expr(env: Env, e: Expr) -> (Type, Effect, Option<Pred>, Env, Option<Constraint>) {
  match e {
    Expr::Const(c) => {
      let t = match c {
        Const::Number(_) => Type::Number,
        Const::String(_) => Type::String,
        Const::Undef     => Type::Void,
      };
      (t, Effect::Bottom, None, env, None)
    }

    _ => unimplemented!(),
  }
}

fn cgen(s: Statement) -> (Effect, Env, Option<Constraint>) {
  cgen_statement(HashMap::new(), s)
}

// * main

fn main() {

  let p0 = Statement::Expr(str("nil"));

  let p1 =
    seq(vec![
      var("nil", rec(vec![("kind", str("nil"))])),
      var("cons", func(vec!["head", "tail"], skip, rec(vec![("kind", str("cons")),
                                                            ("head", val("head")),
                                                            ("tail", val("tail"))]))),
    ]);

  let p = p0;

  println!("{:?}", p);
  println!("{:?}", cgen(p));
}
