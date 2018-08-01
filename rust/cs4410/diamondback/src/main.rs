extern crate diamondback;

use std::io::{self, Read};

use diamondback::lex::TokenStream;
use diamondback::parse::parse;
use diamondback::compile::compile;

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let ast = parse(&mut TokenStream::new(&input));
  println!("{}", compile(ast));
}
