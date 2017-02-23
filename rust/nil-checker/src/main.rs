use std::fmt;
use std::cmp;

enum Node<'a> {
  Sexp(Vec<Node<'a>>),
  Atom(AtomType<'a>),
}

#[derive(Copy, Clone)]
enum AtomType<'a> {
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

impl<'a> fmt::Display for AtomType<'a> {
  fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    match self {
      &AtomType::Str(ref str) => write!(formatter, "{}", str),
      &AtomType::Num(ref n) => write!(formatter, "{}", n),
      &AtomType::Nil => write!(formatter, "nil"),
    }
  }
}

fn parse(prog: &str) -> Node {
  Node::Atom(AtomType::Num(42))
}

#[derive(Debug)]
enum TypeError {
  Err(String),
  ArityMismatch(String),
  NotAFunction(String),
  ArgMismatch(String),
}

#[derive(PartialEq, Debug)]
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

fn subtype(a: &Type, b: &Type) -> bool {
  if let Type::Atom(ConcreteType::MaybeNil) = *b {
    true
  } else {
    false
  }
}

fn node_type<'a>(node: &Node) -> Result<Type, TypeError> {
  match *node {
    Node::Sexp(ref nodes) => {
      let f = &nodes[0];
      let args = &nodes[1];
      let tf = node_type(&f);
      // if arity(tf) != args.length() {
      //   Err(TypeError::ArityMismatch(format!("{} != {}", arity(tf), args.length))
      // }

      match tf? {
        Type::Fun(targ, tret) => {
          let ta = node_type(&args)?;
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

    Node::Atom(AtomType::Str("list")) =>
      Ok(Type::Fun(Box::new(Type::Atom(ConcreteType::MaybeNil)),
                ConcreteType::NonNil)),

    Node::Atom(AtomType::Str("car")) =>
      Ok(Type::Fun(Box::new(Type::Atom(ConcreteType::NonNil)),
                ConcreteType::MaybeNil)),

    Node::Atom(AtomType::Nil) => Ok(Type::Atom(ConcreteType::Nil)),
      _ => Ok(Type::Atom(ConcreteType::NonNil)),
  }
}

fn main() {
  let p1 = Node::Sexp(vec![Node::Atom(AtomType::Str("car".into())),
                           Node::Sexp(vec![Node::Atom(AtomType::Str("list".into())),
                                           Node::Atom(AtomType::Num(1))])]);
  println!("{}", p1);
  println!("{:?}", node_type(&p1));

  let p2 = Node::Sexp(vec![Node::Atom(AtomType::Str("car".into())),
                           Node::Atom(AtomType::Num(1))]);

  println!("{}", p2);
  println!("{:?}", node_type(&p2));

  let p3 = Node::Sexp(vec![Node::Atom(AtomType::Str("car".into())),
                           Node::Atom(AtomType::Nil)]);

  println!("{}", p3);
  println!("{:?}", node_type(&p3));
}
