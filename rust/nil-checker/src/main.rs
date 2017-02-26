use std::fmt;
use std::cmp;
use std::collections::BTreeMap;

enum Node<'a> {
  Sexp(Vec<Node<'a>>),
  Atom(Atom<'a>),
}

#[derive(Copy, Clone)]
enum Atom<'a> {
  Str(&'a str),
  Num(u32),
  Nil,
}

impl<'a> fmt::Display for Node<'a> {
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Node::Sexp(ref nodes) => {
        write!(formatter, "(");
        for n in nodes {
          write!(formatter, "{} ", n);
        }
        write!(formatter, ")")
      },
      &Node::Atom(ref a) => write!(formatter, "{}", a),
    }
  }
}

impl<'a> fmt::Display for Atom<'a> {
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &Atom::Str(ref str) => write!(formatter, "{}", str),
      &Atom::Num(ref n) => write!(formatter, "{}", n),
      &Atom::Nil => write!(formatter, "nil"),
    }
  }
}

fn parse(prog: &str) -> Node {
  Node::Atom(Atom::Num(42))
}

#[derive(Debug)]
enum TypeError {
  Err(String),
  ArityMismatch(String),
  NotAFunction(String),
  ArgMismatch(String),
  UndefinedName(String),
}

#[derive(PartialEq, Debug, Clone)]
enum Type {
  Fun(Box<Type>, ConcreteType),
  Atom(ConcreteType),
}

enum ParamType {
  Var,
  T,
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum ConcreteType {
  MaybeNil,
  NonNil,
  Nil,
}

// _ <: MaybeNil
fn subtype(a: &Type, b: &Type) -> bool {
  if let Type::Atom(ConcreteType::MaybeNil) = *b {
    true
  } else {
    false
  }
}

type Environment<'a> = BTreeMap<&'a str, Type>;

fn type_of<'a>(node: &'a Node<'a>, ctxt: &'a Environment<'a>) -> Result<Type, TypeError> {
  match *node {
    Node::Sexp(ref nodes) => {
      let f = &nodes[0];
      let args = &nodes[1];
      let tf = type_of(&f, &ctxt)?;
      // if arity(tf) != args.length() {
      //   Err(TypeError::ArityMismatch(format!("{} != {}", arity(tf), args.length))
      // }

      match tf {
        Type::Fun(targ, tret) => {
          let ta = type_of(&args, &ctxt)?;
          if *targ == ta || subtype(&ta, &targ) {
            Ok(Type::Atom(tret))
          } else {
            Err(TypeError::ArgMismatch(
              format!("incorrect arg type, expected {:?}, got {:?}",
                      targ, ta)))
          }
        },

        _ => Err(TypeError::NotAFunction("calling a non-function".into())),
      }
    },

    Node::Atom(Atom::Str(ref a)) => {
      if ctxt.contains_key(a) {
        Ok(ctxt[a].clone())
      } else {
        Err(TypeError::UndefinedName(format!("undefined name, {}", *a)))
      }
    },

    Node::Atom(Atom::Num(_)) => Ok(Type::Atom(ConcreteType::NonNil)),
    Node::Atom(Atom::Nil) => Ok(Type::Atom(ConcreteType::Nil)),
  }
}

fn main() {
  let mut env = BTreeMap::new();
  env.insert("list", Type::Fun(Box::new(Type::Atom(ConcreteType::MaybeNil)),
                               ConcreteType::NonNil));
  env.insert("car", Type::Fun(Box::new(Type::Atom(ConcreteType::NonNil)),
                              ConcreteType::MaybeNil));

  {
    let p = Node::Sexp(vec![Node::Atom(Atom::Str("car".into())),
                            Node::Sexp(vec![Node::Atom(Atom::Str("list".into())),
                                            Node::Atom(Atom::Num(1))])]);
    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Node::Atom(Atom::Str("car".into())),
                            Node::Sexp(vec![Node::Atom(Atom::Str("car".into())),
                                            Node::Atom(Atom::Num(1))])]);
    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Node::Atom(Atom::Str("car".into())),
                            Node::Atom(Atom::Num(1))]);

    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Node::Atom(Atom::Str("car".into())),
                            Node::Atom(Atom::Nil)]);

    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }
}
