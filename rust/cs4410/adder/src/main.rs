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

// Turn a concrete syntax tree into an abstract one
fn abstractify(cst: &ParseTree) -> Expr {
  // Valid adder programs have exactly one root
  if cst.roots.len() == 0 {
    panic!("0:0: unexpected empty program");
  }

  if cst.roots.len() > 1 {
    let n = &cst.roots[1];
    panic!("{}:{}-{}:{}: unexpected top-level expression",
           n.start.line, n.start.column,
           n.end.line, n.end.column);
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
        panic!("{}:{}-{}:{}: expected 'let', 'add1' or 'sub1'",
                n.start.line, n.start.column,
                n.end.line, n.end.column)
      }

      let name = match nodes[0].kind {
        Symbol(s) => symbols.get(s).unwrap(),

        _ => panic!("{}:{}-{}:{}: expected 'let', add1', or 'sub1'",
                    nodes[0].start.line, nodes[0].start.column,
                    nodes[0].end.line, nodes[0].end.column)
      };

      match name.as_str() {
        "let" => if nodes.len() != 3 {
          panic!("{}:{}-{}:{}: 'let' takes exactly 2 arguments, {} provided",
                 nodes[0].start.line, nodes[0].start.column,
                 nodes[0].end.line, nodes[0].end.column,
                 nodes.len() - 1)
        } else {
          build_let(&nodes[1], &nodes[2], symbols)
        }

        "add1" | "sub1" => if nodes.len() != 2 {
          panic!("{}:{}-{}:{}: '{}' takes exactly 1 argument, {} provided",
                 name,
                 nodes[0].start.line, nodes[0].start.column,
                 nodes[0].end.line, nodes[0].end.column,
                 nodes.len() - 1)
        } else {
            Expr::Prim1(match name.as_str() {
              "add1" => Prim1::Add1,
              "sub1" => Prim1::Sub1,
              _ => unreachable!(),
            }, Box::new(build_expr(&nodes[1], symbols)))
        }

        _ => panic!("{}:{}-{}:{}: expected 'let', add1', or 'sub1', found {}",
                    nodes[0].start.line, nodes[0].start.column,
                    nodes[0].end.line, nodes[0].end.column,
                    name)
      }
    }

    _ => panic!("{}:{}-{}:{}: syntax error",
                n.start.line, n.start.column,
                n.end.line, n.end.column)
  }
}

fn build_let(bindings: &Node, body: &Node, symbols: &[String]) -> Expr {
  use NodeKind::*;

  if let Sexp(ref nodes) = bindings.kind {
    Expr::Let(nodes.iter().map(|n| build_binding(n, symbols)).collect(),
              Box::new(build_expr(body, symbols)))
  } else {
    panic!("{}:{}-{}:{}: expected a bindings list",
           bindings.start.line, bindings.start.column,
           bindings.end.line, bindings.end.column)
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
          panic!("{}:{}-{}:{}: invalid binding identifier",
                 id.start.line, id.start.column,
                 id.end.line, id.end.column)
        }

      _ => panic!("{}:{}-{}:{}: expected a binding (id . expr)",
                 nodes[0].start.line, nodes[0].start.column,
                 nodes[0].end.line, nodes[0].end.column)
    }
  } else {
    panic!("{}:{}-{}:{}: expected a binding (id . expr)",
           b.start.line, b.start.column,
           b.end.line, b.end.column)
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
