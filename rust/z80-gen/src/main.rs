#![recursion_limit = "1000"] // Needed for the large EnumFromStr derive
// see https://github.com/DanielKeep/rust-custom-derive/issues/12

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate enum_derive;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde_derive;
extern crate docopt;
extern crate regex;

mod parser;

use docopt::Docopt;
use std::io::{BufRead, BufReader, BufWriter};
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

fn emit_disassembler<W>(ops: &[ParsedOpcode], mut writer: W) where W : std::io::Write {
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

    writeln!(writer, "[0x{:04x}] = {{ {}, \"{:?} {}\", {:?}, {:?} }},",
             o.pattern.code, o.length, o.name, args,
             arg1_type, arg2_type).unwrap();
  }
}

fn emit_op_table<W>(ops: &[ParsedOpcode], mut writer: W) where W : std::io::Write {
  for o in ops {
    writeln!(writer, "[0x{:04x}] = z80_op_nop,", o.pattern.code).unwrap();
  }
}

const USAGE: &'static str = "
A Z80 emulator code generator.

Usage:
  z80-gen -d <disassembly> -c <cpu> <opsfile>
  z80-gen -h

Options:
  -h, --help              Show this help.
  -d <disassembly>        Write the disassembly table to this file.
  -c <cpu>                Write opcode tables to this file.
";

#[derive(Deserialize)]
struct Args {
  arg_opsfile: String,
  flag_d: String,
  flag_c: String,
}

fn main() {
  // Process args
  let args: Args = Docopt::new(USAGE)
    .and_then(|d| d.deserialize())
    .unwrap_or_else(|e| e.exit());

  let f = File::open(&args.arg_opsfile)
    .expect(&format!("Can't open {} file", &args.arg_opsfile));
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

    emit_disassembler(&good_ops, BufWriter::new(File::create(&args.flag_d)
                                                .expect(&format!("Can't open {} file", &args.flag_d))));
    emit_op_table(&good_ops, BufWriter::new(File::create(&args.flag_c)
                                            .expect(&format!("Can't open {} file", &args.flag_c))));
  }
}
