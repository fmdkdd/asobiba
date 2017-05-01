use std::fmt;
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
        write!(formatter, "(")?;
        for n in nodes {
          write!(formatter, "{} ", n)?;
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

// fn parse(prog: &str) -> Node {
//   Node::Atom(Atom::Num(42))
// }

#[derive(Debug)]
enum TypeError {
  // Err(String),
  ArityMismatch(String),
  NotAFunction(String),
  ArgMismatch(String),
  UndefinedName(String),
}

#[derive(PartialEq, Debug, Clone)]
enum Type {
  Fun(Vec<Type>, ConcreteType),
  Atom(ConcreteType),
}

// enum ParamType {
//   Var,
//   T,
// }

#[derive(PartialEq, Debug, Copy, Clone)]
enum ConcreteType {
  MaybeNil,
  NonNil,
  Nil,
}

// _ <: MaybeNil
fn subtype(_: &Type, b: &Type) -> bool {
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
      let args = &nodes[1..];
      let tf = type_of(&f, &ctxt)?;

      match tf {
        Type::Fun(targs, tret) => {
          // Check arity of formal parameters versus actual parameters
          if targs.len() != args.len() {
            return Err(TypeError::ArityMismatch(format!("{} expects {} != {}", f, targs.len(), args.len())))
          }

          // Check that all actual parameters match the type of the formal
          // parameters
          let mut ctx = ctxt.clone();
          for (targ, arg) in targs.iter().zip(args) {
            let ta = type_of(&arg, &ctx)?;
            if *targ != ta && !subtype(&ta, &targ) {
              return Err(TypeError::ArgMismatch(
                format!("incorrect arg type, {} expected {:?}, got {:?}",
                        f, targ, ta)))
            }

            // `and` and `and3` are special in that subsequent uses of the
            // argument ensures it is non-nil
            if let Node::Atom(Atom::Str(f_name)) = *f {
              if f_name == "and3" || f_name == "and" {
                if let Node::Atom(Atom::Str(arg_name)) = *arg {
                  ctx.insert(arg_name, Type::Atom(ConcreteType::NonNil));
                }
              }
            }

          }

          return Ok(Type::Atom(tret))
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
  use Node::*;
  use Atom::*;

  let mut env = BTreeMap::new();
  env.insert("list", Type::Fun(vec![Type::Atom(ConcreteType::MaybeNil)],
                               ConcreteType::NonNil));
  env.insert("car", Type::Fun(vec![Type::Atom(ConcreteType::NonNil)],
                              ConcreteType::MaybeNil));

  {
    let p = Node::Sexp(vec![Atom(Str("car".into())),
                            Sexp(vec![Atom(Str("list".into())),
                                      Atom(Num(1))])]);
    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Atom(Str("car".into())),
                            Sexp(vec![Atom(Str("car".into())),
                                      Atom(Num(1))])]);
    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Atom(Str("car".into())),
                            Atom(Num(1))]);

    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    let p = Node::Sexp(vec![Atom(Str("car".into())),
                            Atom(Nil)]);

    println!("{}", p);
    println!("{:?}", type_of(&p, &env));
  }

  {
    env.insert("file-name", Type::Atom(ConcreteType::MaybeNil));
    env.insert("buffer-file-name", Type::Atom(ConcreteType::MaybeNil));
    env.insert("flycheck-same-files-p", Type::Fun(vec![Type::Atom(ConcreteType::NonNil),
                                                       Type::Atom(ConcreteType::NonNil),],
                                                  ConcreteType::NonNil));
    env.insert("or", Type::Fun(vec![Type::Atom(ConcreteType::MaybeNil),
                                    Type::Atom(ConcreteType::MaybeNil),],
                               ConcreteType::MaybeNil));
    env.insert("and", Type::Fun(vec![Type::Atom(ConcreteType::MaybeNil),
                                     Type::Atom(ConcreteType::MaybeNil),],
                                ConcreteType::MaybeNil));
    env.insert("and3", Type::Fun(vec![Type::Atom(ConcreteType::MaybeNil),
                                      Type::Atom(ConcreteType::MaybeNil),
                                      Type::Atom(ConcreteType::MaybeNil),],
                                 ConcreteType::MaybeNil));
    env.insert("not", Type::Fun(vec![Type::Atom(ConcreteType::MaybeNil),],
                                ConcreteType::MaybeNil));

    // Does not type check, as expected
    let p = Node::Sexp(vec![Atom(Str("or".into())),
                            Sexp(vec![Atom(Str("and".into())),
                                      Sexp(vec![Atom(Str("not".into())),
                                                Atom(Str("file-name".into()))]),
                                      Sexp(vec![Atom(Str("not".into())),
                                                Atom(Str("buffer-file-name".into()))])]),
                            Sexp(vec![Atom(Str("flycheck-same-files-p".into())),
                                      Atom(Str("file-name".into())),
                                      Atom(Str("buffer-file-name".into()))])]);

    println!("{}", p);
    println!("{:?}", type_of(&p, &env));


    // This is safe, since `and` ensures both args are non-nil.  But we need to
    // instruct the type checker that and3 has this behavior
    let p2 = Node::Sexp(vec![Atom(Str("or".into())),
                             Sexp(vec![Atom(Str("and".into())),
                                       Sexp(vec![Atom(Str("not".into())),
                                                 Atom(Str("file-name".into()))]),
                                       Sexp(vec![Atom(Str("not".into())),
                                                 Atom(Str("buffer-file-name".into()))])]),
                             Sexp(vec![Atom(Str("and3")),
                                       Atom(Str("buffer-file-name")),
                                       Atom(Str("file-name")),
                                       Sexp(vec![Atom(Str("flycheck-same-files-p".into())),
                                                 Atom(Str("file-name".into())),
                                                 Atom(Str("buffer-file-name".into()))])])]);

    println!("{}", p2);
    println!("{:?}", type_of(&p2, &env));
  }
}
