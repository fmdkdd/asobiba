extern crate boa;

use std::io::{self, Read};

use boa::lex::TokenStream;
use boa::parse::parse;
use boa::compile::debug;

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let ast = parse(&mut TokenStream::new(&input));
  debug(ast);
}
