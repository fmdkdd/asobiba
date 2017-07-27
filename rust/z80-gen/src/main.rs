#![recursion_limit = "1000"] // Needed for the large EnumFromStr derive
// see https://github.com/DanielKeep/rust-custom-derive/issues/12

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate enum_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;

mod parser;

use std::io::{BufRead, BufReader};
use std::fs::File;
use std::str::FromStr;

use parser::ParsedOpcode;

fn emit_disassembler(ops: &[ParsedOpcode]) {
  for o in ops {
    println!("[0x{:04x}] = {{ {}, \"{}\" }},", o.pattern.code, o.length,  o.raw_mnemonic);
  }
}

fn emit_op_table(ops: &[ParsedOpcode]) {
  for o in ops {
    println!("[0x{:04x}] = z80_op_nop,", o.pattern.code);
  }
}

fn main() {
  let f = File::open("z80_ops.txt").expect("Can't open z80_ops.txt file");
  let b = BufReader::new(f);
  let parsed_ops: Result<Vec<ParsedOpcode>, String> = b.lines()
    .map(|l| ParsedOpcode::from_str(&l.unwrap()))
    .collect();

  if let Err(e) = parsed_ops {
    println!("Parse error: {}", e);
    ::std::process::exit(1);
  } else {
    let ops = parsed_ops.unwrap();
    let good_ops: Vec<ParsedOpcode> = ops.into_iter().filter(|p| !p.undocumented).collect();

    // TODO: compare ops to the raw string
    // for o in good_ops {
    //   println!("{:?}", o);
    // }

    emit_disassembler(&good_ops);
    //emit_op_table(&good_ops);
  }
}
