extern crate cobra;

use std::io::{self, Read};

use cobra::lex::TokenStream;
use cobra::parse::parse;
use cobra::compile::debug;

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let ast = parse(&mut TokenStream::new(&input));
  debug(ast);
}
