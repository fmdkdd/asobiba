extern crate nil_checker;

use std::io::{self, Read};

use nil_checker::parser::{Node, NodeKind, Parser, ParseTree};

#[derive(Debug)]
struct Constraint {
  desc: String,
}

struct Checker<'a> {
  parse_tree: &'a ParseTree,
}

impl<'a> Checker<'a> {
  fn new(p: &'a ParseTree) -> Self {
    Checker {
      parse_tree: p,
    }
  }

  fn emit(&self) -> Vec<Constraint> {
    self.parse_tree.roots.iter().flat_map(Self::emit_node).collect()
  }

  // Emit type constraints
  fn emit_node(node: &Node) -> Vec<Constraint> {
    use NodeKind::*;

    match node.kind {
      Sexp(ref nodes) =>
        vec![Constraint {
          desc: format!("{:?} is a function",
                        nodes[0].kind)
        }],
      _ => Vec::new(),
    }
  }
}

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let mut p = Parser::new(&input);
  let ast = p.parse();
  let chck = Checker::new(&ast);
  let cstr = chck.emit();
  println!("{:?}", cstr);
  println!("{} constraints", cstr.len());
}
