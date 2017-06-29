#![recursion_limit = "1000"] // Needed for the large EnumFromStr derive
// see https://github.com/DanielKeep/rust-custom-derive/issues/12

#[macro_use] extern crate custom_derive;
#[macro_use] extern crate enum_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;

use std::io::{BufRead, BufReader};
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::str::FromStr;

use regex::Regex;

// Micro operations used to define the behavior of opcodes, and that will be
// translated to the target programming language.
#[derive(Copy, Clone, Debug)]
enum MicroOp {
  // Reading a value
  Val(Param),                   // v = P

  // Writing a value
  Assign(Param),                // P = v
}

// A list of low-level operations
struct Block {
  ops: Vec<MicroOp>,
}

impl Block {
  fn new(ops: Vec<MicroOp>) -> Self {
    Block { ops: ops }
  }
}

// Short-hand for translating opcodes
macro_rules! block {
  ( $( $x: expr ), *) => {{
    use MicroOp::*; use R::*;
    Block::new(vec![
      $(
        $x,
      )*
    ])
  }};
}

// Parameters
#[derive(Copy, Clone, Debug)]
struct Param {
  r: R,
  addr: bool,
}

macro_rules! p {
  ( ($x: ident) ) => { Param { r: $x, addr: true } };
  ( $x: ident ) => { Param { r: $x, addr: false }};
}

#[derive(Copy, Clone, Debug)]
enum R {
  A, F, B, C, D, E, H, L, N,
  BC, DE, HL, AF, SP, NN,
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Translating opcodes to micro operations

fn LD(p1: Param, p2: Param) -> Block {
  block![
    Val(p2),
    Assign(p1)
  ]
}

// fn ld_rr_nn_addr(rr: R) -> Block {
//   block![
//     Read(NN),
//     Addr(rr),
//     Write(rr)
//   ]
// }

// fn ld_rr_a(rr: R) -> Block {
//   block![
//     Val(A),
//     Assign(rr)
//   ]
// }


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Generating the switch-case

fn r_to_x(r: R) -> Option<u16> {
  use R::*;
  match r {
    B  => Some(0),
    C  => Some(1),
    D  => Some(2),
    E  => Some(3),
    H  => Some(4),
    L  => Some(5),
    HL => Some(6),
    A  => Some(7),
    _  => None,
  }
}

fn emit_loads() {
  use R::*;

  // 0x40 to 0x7F
  for r1 in vec![B, C, D, E, H, L, A].into_iter() {
    for r2 in vec![B, C, D, E, H, L, HL, A].into_iter() {
      let op = 0x40 | (r_to_x(r1).unwrap() << 3) | r_to_x(r2).unwrap();
      match r2 {
        HL => emit(op, LD(p!(r1), p!((HL)))),
        _ => emit(op, LD(p!(r1), p!(r2))),
      }
    }
  }

  // 0x70 to 0x75
  for r2 in vec![B, C, D, E, H, L, A].into_iter() {
    let op = 0x40 | (r_to_x(HL).unwrap() << 3) | r_to_x(r2).unwrap();
    emit(op, LD(p!((HL)), p!(r2)));
  }

  // 0x*1
  for (c, r1) in vec![(0x00, BC), (0x10, DE), (0x20, HL), (0x30, SP)].into_iter() {
    emit(0x01 | c, LD(p!(r1), p!(NN)));
  }

  // 0x*2 and 0x*A
  for (c, r1, r2) in vec![(0x00, BC, A), (0x10, DE, A),
                          (0x20, NN, HL), (0x30, NN, A)].into_iter() {
    emit(0x02 | c, LD(p!((r1)), p!(r2)));
    emit(0x0A | c, LD(p!(r2), p!((r1))));
  }

  // 0x*6 and 0x*F
  for r in vec![B, C, D, E, H, L, HL, A].into_iter() {
    let op = 0x06 | (r_to_x(r).unwrap() << 3);
    match r {
      HL => emit(op, LD(p!((HL)), p!(N))),
      _ => emit(op, LD(p!(r), p!(N))),
    }
  }
  // emit(0x02 | c, LD(p!((r1)), p!(A)), 3);



}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Translating the switch-case to C

impl Display for R {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use R::*;
    match *self {
      A  => write!(f, "reg.a"),
      F  => write!(f, "reg.f"),
      B  => write!(f, "reg.b"),
      C  => write!(f, "reg.c"),
      D  => write!(f, "reg.d"),
      E  => write!(f, "reg.e"),
      H  => write!(f, "reg.h"),
      L  => write!(f, "reg.l"),
      AF => write!(f, "reg.af"),
      BC => write!(f, "reg.bc"),
      DE => write!(f, "reg.de"),
      HL => write!(f, "reg.hl"),
      SP => write!(f, "reg.sp"),
      N  => write!(f, "reg.pc++"),
      NN => write!(f, "read16()"),
    }
  }
}

impl Display for MicroOp {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use MicroOp::*;
    match *self {
      Val(p) => {
        if p.addr {
          write!(f, "addr = {};", p.r)?;
          write!(f, " v = read(addr);")?;
          Ok(())
        } else {
          write!(f, " v = {};", p.r)?;
          Ok(())
        }
      }
      Assign(p) => {
        if p.addr {
          write!(f, "write({}, v);", p.r)
        } else {
          write!(f, "{} = v;", p.r)
        }
      }
    }
  }
}

impl Display for Block {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for op in self.ops.iter() {
      write!(f, "{} ", op)?
    }
    Ok(())
  }
}

fn emit(opcode: u16, b: Block) {
  println!("case 0x{:02x}: {{ {} }} break;", opcode, b);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parse z80_ops file

#[derive(Debug)]
struct ParsedOpcode {
  raw: String,                  // the unparsed line
  code: u16,
  name: Mnemonic,
  dst: Option<ParsedOperand>,
  src: Option<ParsedOperand>,
  undocumented: bool,
  raw_mnemonic: String,
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

      "PC + n" => AddressRelative,

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
        Regex::new(r"^([0-9ABCDEF]{2,4})(?: [nd])?(?: (?:[nd]|[0-9ABCDEF]{2}))?   *(.*)$")
        .unwrap();
    }

    let raw = s.to_owned();
    let caps = RE.captures(&s)
      .ok_or(format!("malformed opcode string '{}'", s))?;
    let code = u16::from_str_radix(&caps[1], 16)
      .map_err(|_| format!("failed to parse hex number '{}' in '{}'", &caps[1], s))?;
    let mnemonic = ParsedMnemonic::from_str(&caps[2])?;

    Ok(ParsedOpcode {
      raw, code,
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
    println!("[0x{:04x}] = \"{}\",", o.code, o.raw_mnemonic);
  }
}

fn emit_op_table(ops: &[ParsedOpcode]) {
  for o in ops {
    println!("[0x{:04x}] = z80_op_nop,", o.code);
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
    emit_op_table(&good_ops);
  }
}
