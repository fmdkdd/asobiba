use std::io::{self, Read};

mod parser;

use parser::{Node, NodeKind, ParseTree, Parser};


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Abstract syntax

#[derive(Debug)]
enum Prim1 {
  Add1,
  Sub1,
}

#[derive(Debug)]
enum Expr {
  Number(i32),
  Id(String),
  Prim1(Prim1, Box<Expr>),
  Let(Vec<(String, Expr)>, Box<Expr>),
}

fn loc(n : &Node) -> String {
  format!("{}:{}", n.start.line, n.start.column)
}

// Turn a concrete syntax tree into an abstract one
fn abstractify(cst: &ParseTree) -> Expr {
  // Valid adder programs have exactly one root
  if cst.roots.len() == 0 {
    panic!("0:0: unexpected empty program");
  }

  if cst.roots.len() > 1 {
    let n = &cst.roots[1];
    panic!("{}: unexpected top-level expression", loc(n));
  }

  build_expr(&cst.roots[0], &cst.symbol_table)
}

fn build_expr(n: &Node, symbols: &[String]) -> Expr {
  use NodeKind::*;

  match n.kind {
    Number(num) => Expr::Number(num as i32),
    Symbol(s) => Expr::Id(symbols.get(s).unwrap().clone()),

    Sexp(ref nodes) => {
      if nodes.len() == 0 {
        panic!("{}: expected 'let', 'add1' or 'sub1'", loc(n))
      }

      let name = match nodes[0].kind {
        Symbol(s) => symbols.get(s).unwrap(),

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

fn build_binding(b: &Node, symbols: &[String]) -> (String, Expr) {
  use NodeKind::*;

  if let Sexp(ref nodes) = b.kind {
    match nodes.as_slice() {
      [id, body] =>
        if let Symbol(s) = id.kind {
          (symbols.get(s).unwrap().clone(), build_expr(body, symbols))
        } else {
          panic!("{} invalid binding identifier", loc(id))
        }

      _ => panic!("{}: expected a binding (id . expr)", loc(&nodes[0]))
    }
  } else {
    panic!("{}: expected a binding (id . expr)", loc(b))
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compiler


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// main

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let mut p = Parser::new(&input);
  let cst = p.parse();
  let ast = abstractify(&cst);

  println!("{:?}", ast);
}
