#[macro_use] extern crate lazy_static;
extern crate regex;

use std::io::{BufRead, BufReader};
use std::fmt::{self, Display, Formatter};
use std::fs::File;

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
  arg1: Option<OpcodeArg>,
  arg2: Option<OpcodeArg>,
  mnemonic: ParsedMnemonic,
}

#[derive(Debug)]
struct ParsedMnemonic {
  raw: String,
  name: String,
  dst: Operand,
  src: Operand,
}

#[derive(Debug)]
struct Operand {
}

#[derive(Debug)]
enum OpcodeArg {
  ImmUnsigned,
  ImmSigned,
  Code(u8),
}

fn parse_z80_op_arg(arg: &str) -> OpcodeArg {
  println!("{}", arg);
  match arg {
    "n" => OpcodeArg::ImmUnsigned,
    "d" => OpcodeArg::ImmSigned,
    _   => OpcodeArg::Code(u8::from_str_radix(arg, 16).unwrap()),
  }
}

fn parse_z80_mnemonic(input: &str) -> ParsedMnemonic {
  let raw = input.to_owned();

  ParsedMnemonic {
    raw,
    name: String::from("FOO"),
    dst: Operand {},
    src: Operand {},
  }
}

fn parse_z80_op_line(line: String) -> ParsedOpcode {
  lazy_static! {
    static ref RE: Regex =
      Regex::new(r"^([0-9ABCDEF]{2,4})(?: ([nd]))?(?: ([nd]|[0-9ABCDEF]{2}))?   *(.*)$")
      .unwrap();
  }

  let raw = line.to_owned();
  let caps = RE.captures(&line).unwrap();

  let op = ParsedOpcode {
    raw,
    code: u16::from_str_radix(&caps[1], 16).unwrap(),
    arg1: caps.get(2).map(|t| parse_z80_op_arg(t.as_str())),
    arg2: caps.get(3).map(|t| parse_z80_op_arg(t.as_str())),
    mnemonic: parse_z80_mnemonic(&caps[4]),
  };

  op
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main


fn main() {
  let f = File::open("z80_ops.txt").expect("Can't open z80_ops.txt file");
  let b = BufReader::new(f);
  let parsed_ops: Vec<ParsedOpcode> = b.lines()
    .map(|l| l.unwrap())
    .map(parse_z80_op_line)
    .collect();

  for p in parsed_ops {
    println!("{:?}", p);
  }
}
