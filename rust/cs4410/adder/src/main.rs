use std::io::{self, Read};
use std::fmt::Display;

mod parser;

use parser::{Node, NodeKind, ParseTree, Parser};


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Abstract syntax

#[derive(Debug, PartialEq)]
enum Prim1 {
  Add1,
  Sub1,
}

#[derive(Debug, PartialEq)]
enum Expr {
  Number(i32),
  Id(usize),
  Prim1(Prim1, Box<Expr>),
  Let(Vec<(usize, Expr)>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
struct AST {
  prog: Expr,
  symbols: Vec<String>,
}

// Helper for printing location of errors
fn loc(n : &Node) -> String {
  format!("{}:{}", n.start.line, n.start.column)
}

// Turn a concrete syntax tree into an abstract one
fn abstractify(cst: &ParseTree) -> AST {
  // Valid adder programs have exactly one root
  if cst.roots.is_empty() {
    panic!("0:0: unexpected empty program");
  }

  if cst.roots.len() > 1 {
    let n = &cst.roots[1];
    panic!("{}: unexpected top-level expression", loc(n));
  }

  AST {
    prog: build_expr(&cst.roots[0], &cst.symbol_table),
    symbols: cst.symbol_table.clone(),
  }
}

fn build_expr(n: &Node, symbols: &[String]) -> Expr {
  use NodeKind::*;

  match n.kind {
    Number(num) => Expr::Number(num as i32),
    Symbol(s) => Expr::Id(s),

    Sexp(ref nodes) => {
      if nodes.is_empty() {
        panic!("{}: expected 'let', 'add1' or 'sub1'", loc(n))
      }

      let name = match nodes[0].kind {
        Symbol(s) => &symbols[s],

        _ => panic!("{}: expected 'let', add1', or 'sub1'", loc(&nodes[0]))
      };

      match name.as_str() {
        "let" => if nodes.len() != 3 {
          panic!("{}: 'let' takes exactly 2 arguments, {} provided",
                 loc(&nodes[0]), nodes.len() - 1)
        } else {
          build_let(&nodes[1], &nodes[2], symbols)
        }

        "add1" | "sub1" => if nodes.len() != 2 {
          panic!("{}: '{}' takes exactly 1 argument, {} provided",
                 loc(&nodes[0]), name, nodes.len() - 1)
        } else {
            Expr::Prim1(match name.as_str() {
              "add1" => Prim1::Add1,
              "sub1" => Prim1::Sub1,
              _ => unreachable!(),
            }, Box::new(build_expr(&nodes[1], symbols)))
        }

        _ => panic!("{}: expected 'let', add1', or 'sub1', found {}",
                    loc(&nodes[0]), name)
      }
    }

    _ => panic!("{}: syntax error", loc(n))
  }
}

fn build_let(bindings: &Node, body: &Node, symbols: &[String]) -> Expr {
  use NodeKind::*;

  if let Sexp(ref nodes) = bindings.kind {
    Expr::Let(nodes.iter().map(|n| build_binding(n, symbols)).collect(),
              Box::new(build_expr(body, symbols)))
  } else {
    panic!("{}: expected a bindings list", loc(bindings))
  }
}

fn build_binding(b: &Node, symbols: &[String]) -> (usize, Expr) {
  use NodeKind::*;

  if let Sexp(ref nodes) = b.kind {
    match nodes.as_slice() {
      [id, body] =>
        if let Symbol(s) = id.kind {
          (s, build_expr(body, symbols))
        } else {
          panic!("{} invalid binding identifier", loc(id))
        }

      _ => panic!("{}: expected a binding (id . expr)", loc(&nodes[0]))
    }
  } else {
    panic!("{}: expected a binding (id . expr)", loc(b))
  }
}

#[cfg(test)]
mod tests {
  use super::{AST, abstractify, Parser};
  use Expr::*;
  use Prim1::*;

  macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
  }

  #[test]
  fn number() {
    let input = "5";
    let expected = AST {
      prog: Number(5),
      symbols: vec![],
    };

    let mut p = Parser::new(input);
    let ast = abstractify(&p.parse());
    assert_eq!(expected, ast);
  }

  #[test]
  fn prim1() {
    let input = "(sub1 (add1 (sub1 5)))";
    let expected = AST {
      prog: Prim1(Sub1, Box::new(Prim1(Add1, Box::new(Prim1(Sub1, Box::new(Number(5))))))),
      symbols: vec_of_strings!("sub1", "add1"),
    };

    let mut p = Parser::new(input);
    let ast = abstractify(&p.parse());
    assert_eq!(expected, ast);
  }

  #[test]
  fn let1() {
    let input = "(let ((x 5))
                   (add1 x))";
    let expected = AST {
      prog: Let(vec![(1, Number(5))],
                Box::new(Prim1(Add1, Box::new(Id(1))))),
      symbols: vec_of_strings!("let", "x", "add1"),
    };

    let mut p = Parser::new(input);
    let ast = abstractify(&p.parse());
    assert_eq!(expected, ast);
  }

  #[test]
  fn let2() {
    let input = "(let ((x 5)
                       (y (sub1 x)))
                   (sub1 y))";
    let expected = AST {
      prog: Let(vec![(1, Number(5)),
                     (2, Prim1(Sub1, Box::new(Id(1))))],
                Box::new(Prim1(Sub1, Box::new(Id(2))))),
      symbols: vec_of_strings!("let", "x", "y", "sub1"),
    };

    let mut p = Parser::new(input);
    let ast = abstractify(&p.parse());
    assert_eq!(expected, ast);
  }

}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compiler

#[derive(Debug)]
enum Reg {
  EAX,
  ESP,
}

#[derive(Debug)]
enum Arg {
  Const(i32),
  Reg(Reg),
  RegOffset(Reg, usize),
}

#[derive(Debug)]
enum Instr {
  Mov(Arg, Arg),
  Inc(Arg),
  Dec(Arg),
}

fn lookup(id: usize, env: &[usize]) -> Option<usize> {
  env.iter().position(|&n| n == id).map(|n| n+1)
}

fn compile(ast: &AST) -> Vec<Instr> {
  compile_expr(&ast.prog, &ast.symbols, &mut vec![])
}

fn compile_expr(e: &Expr, symbols: &[String], env: &Vec<usize>) -> Vec<Instr> {
  use Instr::*;
  use Prim1::*;
  use Arg::*;
  use Reg::*;
  use Expr::*;

  match e {
    Number(n) => vec![Mov(Reg(EAX), Const(*n))],

    Prim1(Add1, ex) => {
      let mut v = compile_expr(ex, symbols,env);
      v.push(Inc(Reg(EAX)));
      v
    }

    Prim1(Sub1, ex) => {
      let mut v = compile_expr(ex, symbols, env);
      v.push(Dec(Reg(EAX)));
      v
    }

    Id(s) => match lookup(*s, env) {
      Some(n) => vec![Mov(Reg(EAX), RegOffset(ESP, n))],
      None => panic!("Identifier not bound '{}'", symbols[*s]),
    }

    Let(bindings, body) => {
      let mut env2 = env.clone();
      let mut v = Vec::new();
      for (x, ex) in bindings {
        v.append(&mut compile_expr(ex, symbols, &env2));
        env2.push(*x);
        v.push(Mov(RegOffset(ESP, env2.len()), Reg(EAX)));
      }

      v.append(&mut compile_expr(body, symbols, &mut env2));
      v
    }
  }
}

fn emit_asm(instrs: &[Instr]) -> String {
  format!("section.text
global entry_point
entry_point:
  {}
  ret", instrs.iter().map(|i| format!("{}", i)).collect::<String>())
}

impl Display for Reg {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Reg::*;

    match self {
      EAX => write!(f, "eax"),
      ESP => write!(f, "esp"),
    }
  }
}

impl Display for Arg {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Arg::*;

    match self {
      Const(n) => write!(f, "{}", n),
      Reg(r) => write!(f, "{}", r),
      RegOffset(r, o) => write!(f, "[{} - 4*{}]", r, o),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Instr::*;

    match self {
      Mov(dst, src) => writeln!(f, "mov {}, {}", dst, src),
      Inc(dst) => writeln!(f, "inc {}", dst),
      Dec(dst) => writeln!(f, "dec {}", dst),
    }
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// main

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let mut p = Parser::new(&input);
  let cst = p.parse();
  let ast = abstractify(&cst);
  println!("{}", emit_asm(&compile(&ast)));
}
