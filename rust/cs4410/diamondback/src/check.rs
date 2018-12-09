use crate::parse::{AST, Expr};

// TODO: change run-tests script to check for static errors

pub enum CheckErrorKind {
  ArityError,
  UnboundFun,
}

pub struct CheckError {
  kind: CheckErrorKind,
  msg: String,
  filename: String,
  line: usize,
  column: usize,
}

// impl CheckError {
//   fn new() -> Self {
//     CheckError {
//     }
//   }
// }

impl core::fmt::Display for CheckError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}:{}:{}:error: {}",
           self.filename, self.line, self.column, self.msg)
  }
}

/// Report all static errors in program AST.
pub fn check(ast: &AST<()>) -> Vec<CheckError> {
  use self::CheckErrorKind::*;

  let mut errors = Vec::new();

  // For each Call, check there is one matching Decl
  // TODO: populate that from the Runtime enum to stay DRY
  let builtins = [
    "add1", "sub1",
    "print",
    "isbool", "isnum",
    ];

  let calls = all_of(ast, &Expr::Apply(0, vec![], ()));
  let decls = &ast.root.decls;
  for c in calls {
    if let Expr::Apply(name, _, _) = c {
      if decls.iter().all(|d| &d.name != name)
        && builtins.iter().all(|b| *b != ast.symbols[*name]) {
        errors.push(CheckError {
          kind: UnboundFun,
          msg: format!("Calling unknown function `{}`", ast.symbols[*name]),

          // TODO: keep this info when parsing
          filename: "<stdin>".to_string(),
          line: 0,
          column: 0,
        });
      }
    }
  }

  errors
}


/// Return all instances of EXPR in AST
fn all_of<'a, T>(ast: &'a AST<T>, expr: &Expr<T>) -> Vec<&'a Expr<T>> {
  use self::Expr::*;

  let prog = &ast.root;

  // Collect root exprs
  let mut roots = prog.decls.iter()
    .map(|d| &d.body).collect::<Vec<&Expr<T>>>();
  roots.push(&prog.body);

  let mut instances = Vec::new();
  for e in roots {


    instances.append(&mut all_of1(e, expr));
  }

  instances
}

/// Return all expressions in BODY of the same variant as EXPR, recursively.
fn all_of1<'a, T>(e: &'a Expr<T>, expr: &Expr<T>) -> Vec<&'a Expr<T>> {
  use self::Expr::*;

  let mut instances = match e {
    Prim1(_, sub, _) => all_of1(sub, expr),

    Prim2(_, left, right, _) => {
      let mut v = all_of1(left, expr);
      v.append(&mut all_of1(right, expr));
      v
    },

    Apply(_, args, _) => {
      let mut v = Vec::new();
      for a in args {
        v.append(&mut all_of1(a, expr));
      }
      v
    },

    Let(bindings, body, _) => {
      let mut v = Vec::new();
      for (_, b) in bindings {
        v.append(&mut all_of1(b, expr));
      }
      v.append(&mut all_of1(body, expr));
      v
    },

    If(cond, then, els, _) => {
      let mut v = all_of1(cond, expr);
      v.append(&mut all_of1(then, expr));
      v.append(&mut all_of1(els, expr));
      v
    },

    _ => vec![]

  };

  if variant_eq(e, expr) {
    instances.push(e);
  }

  instances
}

/// Whether two values are the same variants of an enum
fn variant_eq<T>(a: &T, b: &T) -> bool {
  std::mem::discriminant(a) == std::mem::discriminant(b)
}
