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

use parser::{ParsedOpcode, ParsedOperand};

#[derive(Debug)]
enum ArgType {
  None,
  Unsigned,
  UnsignedWord,
  Signed,
  Relative,
}

fn emit_disassembler_arg(arg: &ParsedOperand) -> (String, ArgType) {
  use ParsedOperand::*;
  use ArgType::*;

  match arg {
    &Bit(ref b)             => (format!("{}", b), None),
    &Register(ref r)        => (format!("{}", r), None),
    &Immediate              => (String::from("$%02x"), Unsigned),
    &ImmediateExtended      => (String::from("$%04x"), UnsignedWord),
    &ZeroPage(n)            => (format!("${:02x}", n), Unsigned),
    &AddressRelative        => (String::from("PC%+d [$%04x]"), Relative),
    &AddressImmediate       => (String::from("($%02x)"), Unsigned),
    &AddressExtended        => (String::from("($%04x)"), UnsignedWord),
    &AddressRegister(ref r) => (format!("({:?})", r), None),
    &AddressIndexed(ref r)  => (format!("({:?} + %d)", r), Signed),
    &Conditional(ref c)     => (format!("{:?}", c), None),
    _                       => (format!("{:?}", arg), None),
  }
}

fn emit_disassembler(ops: &[ParsedOpcode]) {
  // For each opcode, we emit a struct that gives the necessary data for a
  // common function to produce a disassembly string
  for o in ops {
    let mut args = String::new();
    let mut arg1_type = ArgType::None;
    if let Some(ref arg) = o.dst {
      let (s, t) = emit_disassembler_arg(arg);
      args += &s;
      arg1_type = t;
    }
    let mut arg2_type = ArgType::None;
    if let Some(ref arg) = o.src {
      args += ", ";
      let (s, t) = emit_disassembler_arg(arg);
      args += &s;
      arg2_type = t;
    }

    println!("[0x{:04x}] = {{ {}, \"{:?} {}\", {:?}, {:?} }},",
             o.pattern.code, o.length, o.name, args,
             arg1_type, arg2_type);
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
    let good_ops: Vec<ParsedOpcode> = ops.into_iter()
      // Ignore undocumented opcodes
      .filter(|p| !p.undocumented)
      // Ignore CB prefix opcode
      .filter(|p| !(p.pattern.code & 0xFF == 0xCB))
      .collect();

    // TODO: compare ops to the raw string
    // for o in good_ops {
    //   println!("{:?}", o);
    // }

    emit_disassembler(&good_ops);
    //emit_op_table(&good_ops);
  }
}
