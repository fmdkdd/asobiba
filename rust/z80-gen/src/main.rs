#![recursion_limit = "1000"] // Needed for the large EnumFromStr derive
// see https://github.com/DanielKeep/rust-custom-derive/issues/12

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate enum_derive;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde_derive;
extern crate docopt;
extern crate regex;
extern crate strfmt;

mod parser;

use docopt::Docopt;
use std::collections::HashSet;
use std::io::{BufRead, BufReader, BufWriter};
use std::fs::File;
use std::str::FromStr;

use parser::{Condition, Mnemonic, ParsedOpcode, ParsedOperand};

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

fn emit_disassembler<W>(ops: &[ParsedOpcode], writer: &mut W) where W : std::io::Write {
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


mod func_utils {
  use parser::ParsedOperand;

  // Used for function name
  pub fn label(arg: &Option<ParsedOperand>) -> String {
    use ParsedOperand::*;

    match arg {
      &Some(Bit(b)) => format!("b_{}", b),
      &Some(Register(ref src)) => format!("{}", src).replace("'", "2").to_lowercase(),
      &Some(Immediate) => format!("n"),
      &Some(ImmediateExtended) => format!("nn"),
      &Some(ZeroPage(n)) => format!("z_{}", n),
      &Some(AddressRelative) => format!("reladd"),
      &Some(AddressImmediate) => format!("immadd"),
      &Some(AddressExtended) => format!("extadd"),
      &Some(AddressIndexed(ref src)) => format!("{}_d", src).to_lowercase(),
      &Some(AddressRegister(ref src)) => format!("{}_ind", src).to_lowercase(),
      &Some(Conditional(ref cond)) => format!("{:?}", cond).to_lowercase(),

      &None => "".to_string(),

      _ => unreachable!(),
    }
  }

  // Used for reading the operand
  pub fn read(arg: &Option<ParsedOperand>) -> String {
    use ParsedOperand::*;

    match arg {
      &Some(Bit(b)) => format!("src = {}", b),
      &Some(Register(ref src)) => format!("src = z->{}", src).to_lowercase(),
      &Some(Immediate) => format!("src = z->pc++"),
      &Some(ImmediateExtended) => format!(r"lo = z->pc++;
  hi = z->pc++;
  src = hi << 8 | lo;"),
      &Some(ZeroPage(n)) => format!("src = z->ram[{}]", n),
      &Some(AddressRelative) => format!("dis = z->pc++;
  src = z->ram[z->pc + dis]"),
      &Some(AddressImmediate) => format!("src = z->ram[z->pc++]"),
      &Some(AddressExtended) => format!("lo = z->pc++;
  hi = z->pc++;
  src = z->ram[hi << 8 | lo]"),
      &Some(AddressIndexed(ref src)) =>
        format!("src = z->ram[z->{} + (int8_t) z->pc++]", src).to_lowercase(),
      &Some(AddressRegister(ref src)) =>
        format!("src = z->ram[z->{}]", src).to_lowercase(),

      _ => unreachable!(),
    }
  }

  // Used for writing to the operand
  pub fn lval(arg: &Option<ParsedOperand>) -> String {
    use ParsedOperand::*;

    match arg {
      &Some(Register(ref src)) => format!("z->{}", src).to_lowercase(),
      &Some(ZeroPage(n)) => format!("z->ram[{}]", n),
      &Some(AddressRelative) => format!("dis = z->pc++;
  z->ram[z->pc + dis]"),
      &Some(AddressImmediate) => format!("z->ram[z->pc++]"),
      &Some(AddressExtended) => format!("lo = z->pc++;
  hi = z->pc++;
  z->ram[hi << 8 | lo]"),
      &Some(AddressIndexed(ref src)) => format!("z->ram[z->{} + (int8_t) z->pc++]", src).to_lowercase(),
      &Some(AddressRegister(ref src)) => format!("z->ram[z->{}]", src).to_lowercase(),

      _ => unreachable!(),
    }
  }

  pub fn cc(arg: &Option<ParsedOperand>) -> String {
    use ParsedOperand::*;
    use Condition::*;

    match arg {
      &Some(Conditional(ref cond)) => {
        match cond {
          &NZ => format!("z->zf == 0"),
          &Z => format!("z->zf == 1"),
          &NC => format!("z->cf == 0"),
          &Cy => format!("z->cf == 1"),
          &PO => format!("z->pf == 1"),
          &PE => format!("z->pf == 0"),
          &P => format!("z->sf == 0"),
          &M => format!("z->sf == 1"),
        }
      }

      &None => format!("true"),

      _ => unreachable!(),
    }
  }

  pub fn zf1(arg: &str) -> String {
    format!("z->zf = {} == 0;", arg)
  }

  pub fn zf() -> String {
    zf1("res")
  }

  pub fn nf(arg: u8) -> String {
    format!("z->nf = {};", arg)
  }

  pub fn sf1(arg: &str) -> String {
    format!("z->sf = {a} >> (8 * sizeof({a}) - 1);", a = arg)
  }

  pub fn sf() -> String {
    sf1("res")
  }

  pub fn pf1(arg: &str) -> String {
    format!("z->pf = {} & 1;", arg)
  }

  pub fn pf() -> String {
    pf1("res")
  }

  pub fn vf1(arg: &str) -> String {
    // TODO: what the hell is a 2-complement overflow?
    format!("z->pf = {} & 1;", arg)
  }

  pub fn vf() -> String {
    vf1("res")
  }
}

macro_rules! flags {
  ($($x:expr),*) => (vec![$($x),*].join("\n  "));
}

fn emit_op_table<W>(ops: &[ParsedOpcode], wop: &mut W, wfun: &mut W) where W : std::io::Write {
  use Mnemonic::*;
  use ParsedOperand::*;
  use func_utils::*;

  let mut defined = HashSet::new();

  for o in ops {
    let mut cycles = 4; // TODO: compute cycles from addressing modes
    let fname = format!("z80_op_{:?}_{}_{}", o.name, label(&o.dst), label(&o.src)).to_lowercase();

    let body = match (&o.name, &o.dst, &o.src) {
      (&ADD, dst, src) => {
        format!("{};
  res = {} += src;
  {}", read(src), lval(dst), flags!(zf(), nf(0), sf(), vf()))
      }

      (&SUB, s, _) => {
        format!("{};
  res = {} -= src;
  {}", read(s), "z->a", flags!(zf(), nf(1), sf(), pf()))
      }

      // (&INC, _, _) => {
      //   fname = format!("z80_op_inc_{}", label(arg1));

      // }

      (&LD, dst, src) => {
        format!("{};\n  {} = src;", read(src), lval(dst))
      }

      (&PUSH, reg, _) => {
        format!("{};
  z->ram[z->sp-2] = src & 0x00FF;
  z->ram[z->sp-1] = src >> 8;
  z->sp -= 2;", read(reg))
      }

      (&POP, reg, _) => {
        format!("hi = z->ram[z->sp+1];
  lo = z->ram[z->sp];
  {} = hi << 8 | lo;
  z->sp += 2;", lval(reg))
      }

      (&JP, cond @ &Some(Conditional(_)), nn @ &Some(_)) => {
        format!("if ({}) {{
    {};
    z->pc = src;
  }}", cc(cond), read(nn))
      }

      (&JP, nn, &None) => {
        format!("{};\n  z->pc = src;", read(nn))
      }

      _ => {
        format!("")
      }
    };

    // Add it to the ops table
    writeln!(wop, "[0x{:04x}] = {},", o.pattern.code, &fname).unwrap();

    // and write the specialized function definition
    if !defined.contains(&fname) {
      writeln!(wfun, r#"
uint8_t {n}(Z80 *z) {{
  uint16_t src, res, lo, hi;
  int8_t dis;
  {b}
  return {c};
}}"#, n = fname, b = body, c = cycles).unwrap();

      defined.insert(fname);
    }
  }
}

fn emit_funcs_header<W>(w: &mut W) where W : std::io::Write {
  write!(w, r#"
#pragma once

#include <stdint.h>

#include "z80.h"
"#).unwrap();
}

const USAGE: &'static str = "
A Z80 emulator code generator.

Usage:
  z80-gen <opsfile> <disassemblytable> <opstable> <funcs>
  z80-gen -h

Options:
  -h, --help              Show this help.
";

#[derive(Deserialize)]
struct Args {
  arg_opsfile: String,
  arg_disassemblytable: String,
  arg_opstable: String,
  arg_funcs: String,
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

    let mut filename = &args.arg_disassemblytable;
    emit_disassembler(&good_ops,
                      &mut BufWriter::new(File::create(&filename)
                                          .expect(&format!("Can't open {} file", &filename))));

    filename = &args.arg_opstable;
    let mut wops = BufWriter::new(File::create(&filename)
                              .expect(&format!("Can't open {} file", &filename)));
    filename = &args.arg_funcs;
    let mut wfun = BufWriter::new(File::create(&filename)
                              .expect(&format!("Can't open {} file", &filename)));
    emit_funcs_header(&mut wfun);
    emit_op_table(&good_ops, &mut wops, &mut wfun);

  }
}
