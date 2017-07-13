#![recursion_limit = "1000"] // Needed for the large EnumFromStr derive
// see https://github.com/DanielKeep/rust-custom-derive/issues/12

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate enum_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;

use std::io::{BufRead, BufReader};
use std::fs::File;
use std::str::FromStr;

use regex::Regex;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parse z80_ops file

#[derive(Debug)]
struct ParsedOpcode {
  raw: String,                  // the unparsed line
  pattern: ParsedPattern,
  length: u8,
  name: Mnemonic,
  dst: Option<ParsedOperand>,
  src: Option<ParsedOperand>,
  undocumented: bool,
  raw_mnemonic: String,
}

#[derive(Debug)]
struct ParsedPattern {
  code: u16,
  arg1: Option<PatternArg>,
  arg2: Option<PatternArg>,
}


#[derive(Debug)]
enum PatternArg {
  Const(u8),
  Unsigned,
  Signed,
}

#[derive(Debug)]
struct ParsedMnemonic {
  raw: String,
  name: Mnemonic,
  dst: Option<ParsedOperand>,
  src: Option<ParsedOperand>,
  undocumented: bool,
}

#[derive(Debug)]
enum ParsedOperand {
  Bit(u8),
  Register(Register),
  Immediate,
  ImmediateExtended,
  ZeroPage(u8),
  AddressRelative,
  AddressImmediate,
  AddressExtended,
  AddressIndexed(Register),
  AddressRegister(Register),
  Conditional(Condition),

  Unknown,
}

custom_derive! {
  #[derive(Debug, EnumFromStr)]
  enum Mnemonic {
    ADC, ADD, AND, BIT, CALL, CCF, CP, CPD, CPDR, CPI, CPIR, CPL, DAA,
    DEC, DI, DJNZ, EI, EX, EXX, HALT, IM, IN, INC, IND, INDR, INI, INIR,
    JP, JR, LD, LDD, LDDR, LDI, LDIR, NEG, NOP, OR, OTDR, OTIR, OUT, OUTD,
    OUTI, POP, PUSH, RES, RET, RETI, RETN, RL, RLA, RLC, RLCA, RLD, RR, RRA,
    RRC, RRCA, RRD, RST, SBC, SCF, SET, SLA, SLL, SRA, SRL, SUB, XOR,
  }
}

custom_derive! {
  #[derive(Debug, EnumFromStr)]
  enum Register {
    B, C, D, E, H, L, A,
    I, R,
    IXh, IXl, IYh, IYl,
    BC, DE, HL, AF, SP, IX, IY,
    BC_, DE_, HL_, AF_,
  }
}

custom_derive! {
  #[derive(Debug, EnumFromStr)]
  enum Condition {
    NZ, Z, NC, Cy, PO, PE, P, M,
  }
}

impl FromStr for PatternArg {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex =
        Regex::new(r"^[0-9ABCDEF]{2}$").unwrap();
    }
    use PatternArg::*;

    if RE.is_match(s) {
      return Ok(Const(u8::from_str_radix(&s, 16)
                      .map_err(|_| format!("failed to parse hex number '{}'", s))?));
    }

    Ok(match s {
      "n" => Unsigned,
      "e" | "d" => Signed,

      _ => return Err(format!("unknown pattern arg '{}'", s)),
    })
  }
}

impl FromStr for ParsedPattern {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex =
        Regex::new(r"^([0-9ABCDEF]{2,4})(?: ([nde]))?(?: ([nd]|[0-9ABCDEF]{2}))?$")
        .unwrap();
    }

    let caps = RE.captures(&s)
      .ok_or(format!("malformed pattern '{}'", s))?;

    Ok(Self {
      code: u16::from_str_radix(&caps[1], 16)
        .map_err(|_| format!("failed to parse hex number '{}' in '{}'", &caps[1], s))?,
      arg1: caps.get(2).map_or(Ok(None), |t| PatternArg::from_str(t.as_str()).map(Some))?,
      arg2: caps.get(3).map_or(Ok(None), |t| PatternArg::from_str(t.as_str()).map(Some))?,
    })
  }
}

impl FromStr for ParsedOperand {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref COMPOUND: Regex =
        Regex::new(r"^[A-Z]+ ([0-9], )?\([A-Z+ hld]+\)$").unwrap();
    }

    use ParsedOperand::*;
    use Register::*;

    // FIXME: compound ops are ignored, as they are undocumented
    if COMPOUND.is_match(s) {
      return Ok(Unknown);
    }

    Ok(match s {
      "n" => Immediate,
      "A" | "B" | "C" | "D" | "E" |"H" | "L" | "I" | "R"
        => Register(s.parse()
                    .map_err(|_| format!("failed to parse register '{}'", s))?),

      "nn" => ImmediateExtended,
      "BC" | "DE" | "AF" | "HL" | "SP" | "IX" | "IY" | "IXh" | "IXl" | "IYh" | "IYl"
        => Register(s.parse()
                    .map_err(|_| format!("failed to parse register '{}'", s))?),
      "AF'" => Register(AF_),

      "(n)"  => AddressImmediate,
      "(nn)" => AddressExtended,
      "(C)" => AddressRegister(C),
      "(BC)" | "(DE)" | "(HL)" | "(SP)" | "(IX)" | "(IY)"
        => AddressRegister(s[1..s.len()-1].parse()
                           .map_err(|_| format!("failed to parse register '{}'", s))?),

      "(IX + d)" => AddressIndexed(IX),
      "(IY + d)" => AddressIndexed(IY),

      "PC + e" => AddressRelative,

      "NZ" | "Z" | "Cy" | "NC" | "PO" | "PE" | "P" | "M"
        => Conditional(s.parse()
                       .map_err(|_| format!("failed to parse condition '{}'", s))?),

      "0h" | "8h" | "10h" | "18h" | "20h" | "28h" | "30h" | "38h"
        => ZeroPage(u8::from_str_radix(&s[..s.len()-1], 16)
                    .map_err(|_| format!("failed to parse hex number '{}'", s))?),

      "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7"
        => Bit(s.parse()
               .map_err(|_| format!("failed to parse bit '{}'", s))?),

      // FIXME: Weird operand for IM
      "0/1" => Unknown,

      // FIXME: Even weirder
      "(C)* / IN F" => Unknown,

      _ => return Err(format!("unknown operand '{}'", s)),
    })
  }
}

impl FromStr for ParsedMnemonic {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex =
      // eg: LD A, RLC (IX + d)*
        Regex::new(r"([A-Z]+)(?: (.+?))?(?:, (.+?))?(\*)?$").unwrap();
    }

    let raw = s.to_owned();
    let caps = RE.captures(&s)
      .ok_or(format!("malformed mnemonic '{}'", s))?;

    Ok(ParsedMnemonic {
      raw,
      name: Mnemonic::from_str(&caps[1])
        .map_err(|_| format!("failed to parse mnemonic name '{}' in '{}'",
                             &caps[1], s))?,
      // A bit hairy: turns Option<Result<Op, String>> into Result<Option<Op>, String>
      dst: caps.get(2).map_or(Ok(None), |t| ParsedOperand::from_str(t.as_str()).map(Some))?,
      src: caps.get(3).map_or(Ok(None), |t| ParsedOperand::from_str(t.as_str()).map(Some))?,
      undocumented: caps.get(4).is_some(),
    })
  }
}

impl FromStr for ParsedOpcode {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex =
      // eg: DD09 n 78   mnemonic
        Regex::new(r"^([0-9ABCDEF]{2,4}(?: [nde])?(?: (?:[nd]|[0-9ABCDEF]{2}))?)   *(.*)$")
        .unwrap();
    }

    let raw = s.to_owned();
    let caps = RE.captures(&s)
      .ok_or(format!("malformed opcode string '{}'", s))?;
    let pattern = ParsedPattern::from_str(&caps[1])?;
    let length = 1 + (if pattern.arg1.is_some() { 1 } else { 0 })
                   + (if pattern.arg2.is_some() { 1 } else { 0 });
    let mnemonic = ParsedMnemonic::from_str(&caps[2])?;

    Ok(ParsedOpcode {
      raw, pattern, length,
      name: mnemonic.name,
      raw_mnemonic: mnemonic.raw,
      dst: mnemonic.dst,
      src: mnemonic.src,
      undocumented: mnemonic.undocumented,
    })
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Dissambler code

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

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main


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
