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
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader, BufWriter};
use std::fs::File;
use std::str::FromStr;
use strfmt::strfmt;

use parser::{Mnemonic, ParsedOpcode, ParsedOperand};

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

// Used for function name
fn emit_full_arg(arg: &Option<ParsedOperand>) -> String {
  use ParsedOperand::*;

  match arg {
    &Some(Bit(b)) => format!("b_{}", b),
    &Some(Register(ref src)) => format!("{}", src).to_lowercase(),
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
fn emit_src_arg(arg: &Option<ParsedOperand>) -> String {
  use ParsedOperand::*;

  let decl = "uint16_t src =";

  match arg {
    &Some(Bit(b)) => format!("{} {}", decl, b),
    &Some(Register(ref src)) => format!("{} z->{}", decl, src).to_lowercase(),
    &Some(Immediate) => format!("{} z->pc++", decl),
    &Some(ImmediateExtended) => format!(r#"uint16_t lo = z->pc++;
  uint16_t hi = z->pc++;
  {} hi << 8 | lo;"#, decl),
    &Some(ZeroPage(n)) => format!("{} = z->ram[{}]", decl, n),
    &Some(AddressRelative) => format!(r#"int8_t dis = z->pc++;
  {} z->ram[z->pc + dis]"#, decl),
    &Some(AddressImmediate) => format!("{} = z->ram[z->pc++]", decl),
    &Some(AddressExtended) => format!(r#"uint16_t lo = z->pc++;
  uint16_t hi = z->pc++;
  {} z->ram[hi << 8 | lo]"#, decl),
    &Some(AddressIndexed(ref src)) => format!("{} z->ram[z->{} + (int8_t) z->pc++]",
                                              decl, src).to_lowercase(),
    &Some(AddressRegister(ref src)) => format!("{} z->{}", decl, src).to_lowercase(),

    &None => "".to_string(),

    _ => unreachable!(),
  }
}

// Used for writing to the operand
fn emit_dst_arg(arg: &Option<ParsedOperand>) -> String {
  use ParsedOperand::*;

  match arg {
    &Some(Register(ref src)) => format!("z->{}", src).to_lowercase(),
    &Some(ZeroPage(n)) => format!("z->ram[{}]", n),
    &Some(AddressRelative) => format!(r#"int8_t dis = z->pc++;
  z->ram[z->pc + dis]"#),
    &Some(AddressImmediate) => format!("z->ram[z->pc++]"),
    &Some(AddressExtended) => format!(r#"uint16_t lo = z->pc++;
  uint16_t hi = z->pc++;
  z->ram[hi << 8 | lo]"#),
    &Some(AddressIndexed(ref src)) => format!("z->ram[z->{} + (int8_t) z->pc++]", src).to_lowercase(),
    &Some(AddressRegister(ref src)) => format!("z->ram[z->{}]", src).to_lowercase(),
    &Some(Conditional(ref cond)) => {
      match cond {
        _ => format!("bool cc = 0"),
      }
    }

    &None => "".to_string(),

    _ => {
      println!("{:?}", arg);

      unreachable!()
    }
  }
}

fn emit_op_table<W>(ops: &[ParsedOpcode], wop: &mut W, wfun: &mut W) where W : std::io::Write {
  use Mnemonic::*;

  let mut defined = HashSet::new();

  for o in ops {
    let mut fname = "";
    let mut body = "";
    let mut cycles = 4;
    let mut not_yet = false;
    let mut src = &o.src;
    let mut dst = &o.dst;

    match &o.name {
      &ADD => {
        fname = "z80_op_add_{dst}_{src}";
        body = "{src};\n  {dst} += src;"
      }

      &LD => {
        fname = "z80_op_ld_{dst}_{src}";
        body = "{src};\n  {dst} = src;";
      }

      &PUSH => {
        // swap src and dst because PUSH only has one arg
        src = &o.dst;
        dst = &o.src;
        fname = "z80_op_push_{src}";
        body = r#"{src};
  z->ram[z->sp-2] = src & 0x00FF;
  z->ram[z->sp-1] = src >> 8;
  z->sp -= 2;"#;
      }

      &POP => {
        fname = "z80_op_pop_{dst}";
        body = r#"uint16_t hi = z->ram[z->sp+1];
  uint16_t lo = z->ram[z->sp];
  {dst} = hi << 8 | lo;
  z->sp += 2;"#;
      }

      &JP => {
        println!("{:?} {:?} {:?}", o.name, o.dst, o.src);
        src = &o.dst;
        dst = &o.src;
        fname = "z80_op_jp_{dst}_{src}";
        body = r#"{dst};
  if (cc) {{
    {src};
    z->pc = {src};
  }}
"#;
      }

      _ => not_yet = true
    };

    if !not_yet {
      let mut full_vars = HashMap::new();
      full_vars.insert("dst".to_string(), emit_full_arg(dst));
      full_vars.insert("src".to_string(), emit_full_arg(src));
      let fname_str = strfmt(fname, &full_vars).unwrap();

      let mut vars = HashMap::new();
      vars.insert("dst".to_string(), emit_dst_arg(dst));
      vars.insert("src".to_string(), emit_src_arg(src));

      // Write the specialized function definition
      if !defined.contains(&fname_str) {
        writeln!(wfun, r#"
uint8_t {n}(Z80 *z) {{
  {b}
  return {c};
}}"#,
                 n = fname_str,
                 b = strfmt(body, &vars).unwrap(),
                 c = cycles).unwrap();

        defined.insert(fname_str);
      }

      // And add it to the ops table
      writeln!(wop, "[0x{:04x}] = {},",
               o.pattern.code,
               strfmt(fname, &full_vars).unwrap()).unwrap();
    } else {
       writeln!(wop, "[0x{:04x}] = z80_op_nop,", o.pattern.code).unwrap();
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
