// Run static checks to ensure the AST is well-formed.
//
// The point is to collect all errors to give useful feedback to the user.  We
// could collect errors as we compile, but then it's unclear how to keep
// compiling with an ill-formed AST.  It's cleaner and simpler to do all checks
// upfront, and let the compiling code assumes the AST is well-formed, although
// it might not be the most performant solution.

use std::collections::HashSet;
use std::hash::Hash;

use crate::parse::{AST, Expr};

pub enum CheckErrorKind {
  Arity,
  DuplicateFun,
  DuplicateId,
  Overflow,
  UnboundFun,
  UnboundId,
  ShadowId,
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

  // TODO: populate that from the Runtime enum to stay DRY
  let builtins = [
    "add1", "sub1",
    "print",
    "isbool", "isnum",
  ];

  // Check for duplicate function declarations
  let decls = &ast.root.decls;
  let mut uniq = HashSet::new();
  for b in &builtins {
    uniq.insert(*b);
  }

  for d in decls {
    let name = &ast.symbols[d.name];
    // Check for duplicate
    if !uniq.insert(name) {
      errors.push(CheckError {
        kind: DuplicateFun,
        msg: format!("Duplicate declaration of function `{}`", name),

        // TODO: keep this info when parsing
        filename: "<stdin>".to_string(),
        line: 0,
        column: 0,
      });
    }

    // Check argument list is well-formed
    let mut uniq_args = HashSet::new();
    for a in &d.args {
      if !uniq_args.insert(a) {
        errors.push(CheckError {
          kind: DuplicateId,
          msg: format!("Duplicate argument `{}` in declaration of function `{}`",
                       ast.symbols[*a], name),

          // TODO: keep this info when parsing
          filename: "<stdin>".to_string(),
          line: 0,
          column: 0,
        });
      }
    }
  }

  // For each Apply, check there is one matching Decl
  let calls = all_of(ast, &Expr::Apply(0, vec![], ()));
  for c in calls {
    if let Expr::Apply(n, args, _) = c {
      let name = &ast.symbols[*n];
      let actual = args.len();

      // It's a user-defined function
      if let Some(decl) = decls.iter().find(|d| &d.name == n) {
        // Check the arity
        let formal = decl.args.len();

        if formal != actual {
          errors.push(CheckError {
            kind: Arity,
            msg: format!("Function `{}` expects {} arguments, got {}",
                         name, formal, actual),

            // TODO: keep this info when parsing
            filename: "<stdin>".to_string(),
            line: 0,
            column: 0,
          });
        }
      }

      // It's a built-in?
      else if let Some(_) = builtins.iter().find(|b| *b == name) {
        // Check the arity
        let formal = 1; // all builtins have only 1 argument for now

        if formal != actual {
          errors.push(CheckError {
            kind: Arity,
            msg: format!("Function `{}` expects {} arguments, got {}",
                         name, formal, actual),

            // TODO: keep this info when parsing
            filename: "<stdin>".to_string(),
            line: 0,
            column: 0,
          });
        }
      }

      // Then it's undefined!
      else {
        errors.push(CheckError {
          kind: UnboundFun,
          msg: format!("Calling unknown function `{}`", name),

          // TODO: keep this info when parsing
          filename: "<stdin>".to_string(),
          line: 0,
          column: 0,
        });
      }
    }
  }

  // Check for duplicate bindings in Let
  let lets = all_of(ast, &Expr::Let(vec![], Box::new(Expr::Number(0, ())), ()));
  for l in lets {
    if let Expr::Let(bindings, _, _) = l {
      let mut uniq = HashSet::new();
      for (b, _) in bindings {
        if !uniq.insert(b) {
          errors.push(CheckError {
            kind: DuplicateId,
            msg: format!("Duplicate let binding `{}`", ast.symbols[*b]),

            // TODO: keep this info when parsing
            filename: "<stdin>".to_string(),
            line: 0,
            column: 0,
          });
        }
      }
    }
  }

  // Check all numbers are in range
  let nums = all_of(ast, &Expr::Number(0, ()));
  for num in nums {
    if let Expr::Number(n, _) = num {
      if *n > ((1 << 30) - 1) || *n < -(1 << 30) {
        errors.push(CheckError {
          kind: Overflow,
          msg: format!("Integer is too large to be represented"),

          // TODO: keep this info when parsing
          filename: "<stdin>".to_string(),
          line: 0,
          column: 0,
        });
      }
    }
  }

  // Check all identifiers are bound
  errors.append(&mut check_ids(ast));

  errors
}

fn check_ids(ast: &AST<()>) -> Vec<CheckError> {
  let mut errors = Vec::new();

  for decl in &ast.root.decls {
    let mut env = HashSet::new();
    for a in &decl.args {
      env.insert(*a);
    }
    errors.append(&mut check_ids_in_env(&decl.body, &env, &ast.symbols));
  }

  let env = HashSet::new();
  errors.append(&mut check_ids_in_env(&ast.root.body, &env, &ast.symbols));

  errors
}

fn check_ids_in_env(expr: &Expr<()>, env: &HashSet<usize>, symbols: &[String]) -> Vec<CheckError> {
  let mut errors = Vec::new();

  use self::Expr::*;
  match expr {
    Number(_, _) => {}
    Bool(_, _)   => {}

    Prim1(_, e, _) =>
      errors.append(&mut check_ids_in_env(e, env, symbols)),

    Prim2(_, l, r, _) => {
      errors.append(&mut check_ids_in_env(l, env, symbols));
      errors.append(&mut check_ids_in_env(r, env, symbols));
    }

    Apply(_, args, _) => {
      for a in args {
        errors.append(&mut check_ids_in_env(a, env, symbols));
      }
    }

    If(c, t, e, _) => {
      errors.append(&mut check_ids_in_env(c, env, symbols));
      errors.append(&mut check_ids_in_env(t, env, symbols));
      errors.append(&mut check_ids_in_env(e, env, symbols));
    }

    Id(x, _) => {
      if !env.contains(x) {
        errors.push(CheckError {
          kind: CheckErrorKind::UnboundId,
          msg: format!("Unbound identifier `{}`", symbols[*x]),

          // TODO: keep this info when parsing
          filename: "<stdin>".to_string(),
          line: 0,
          column: 0,
        });
      }
    }

    Let(bindings, body, _) => {
      let mut env2 = env.clone();
      for (x, e) in bindings {
        // Check for shadowing
        if env.contains(x) {
          errors.push(CheckError {
            kind: CheckErrorKind::ShadowId,
            msg: format!("Invalid shadowing binding"),

            // TODO: keep this info when parsing
            filename: "<stdin>".to_string(),
            line: 0,
            column: 0,
          });
        }

        env2.insert(*x);
        errors.append(&mut check_ids_in_env(e, &env2, symbols));
      }
      errors.append(&mut check_ids_in_env(body, &env2, symbols));
    }
  }

  errors
}

/// Return all instances of EXPR in AST.
fn all_of<'a, T>(ast: &'a AST<T>, expr: &Expr<T>) -> Vec<&'a Expr<T>> {
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

/// Whether two values are the same variants of an enum.
fn variant_eq<T>(a: &T, b: &T) -> bool {
  std::mem::discriminant(a) == std::mem::discriminant(b)
}
