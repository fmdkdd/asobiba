use std::io::{self, Read};

use diamondback::lex::TokenStream;
use diamondback::parse::parse;
use diamondback::check::check;
use diamondback::compile::compile;

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let ast = parse(&mut TokenStream::new(&input));
  let errors = check(&ast);
  if errors.len() == 0 {
    println!("{}", compile(ast));
  } else {
    for err in &errors {
      eprintln!("{}", err);
    }
    eprintln!("Compilation aborted due to {} errors", errors.len());
    std::process::exit(1);

  }
}
