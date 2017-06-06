use std::fmt::{self, Display, Formatter};

// Micro operations used to define the behavior of opcodes, and that will be
// translated to the target programming language.
enum Op {
  Assign(R),
  Read(R),
  ReadIndirect(R),
}

// A list of low-level operations
struct Block {
  ops: Vec<Op>,
}

#[derive(Copy, Clone, Debug)]
enum R {
  A, F, B, C, D, E, H, L,
  BC, DE, HL, AF,
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Translating opcodes to micro operations

fn ld_r_r(r1: R, r2: R) -> Block {
  use Op::*;
  Block { ops: vec![
    Read(r2),
    Assign(r1)
  ]}
}

fn ld_r_hl(r1: R) -> Block {
  use Op::*;
  Block { ops: vec![
    ReadIndirect(R::HL),
    Assign(r1)
  ]}
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Translating micro operations to C

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
    }
  }
}

impl Display for Op {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    use Op::*;
    match *self {
      Assign(r1) => write!(f, "{} = t;", r1),
      Read(r) => write!(f, "t = {};", r),
      ReadIndirect(r) => write!(f, "t = read({});", r),
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

fn emit(opcode: u16, b: Block, cycles: u16) {
  println!("case 0x{:02x}: {{ {} }} cycles = {}; break;", opcode, b, cycles);
}

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

  for r1 in vec![B, C, D, E, H, L, A].into_iter() {
    for r2 in vec![B, C, D, E, H, L, HL, A].into_iter() {
      let op = 0x40 | (r_to_x(r1).unwrap() << 3) | r_to_x(r2).unwrap();
      match r2 {
        HL => emit(op, ld_r_hl(r1), 2),
        _ => emit(op, ld_r_r(r1, r2), 1),
      }
    }
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main

fn main() {
  emit_loads();
}
