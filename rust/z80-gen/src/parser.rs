use std::fmt;
use std::str::FromStr;

use regex::{Regex, Match};

#[derive(Debug)]
pub struct ParsedOpcode {
  pub raw: String,                  // the unparsed line
  pub pattern: ParsedPattern,
  pub length: u8,
  pub name: Mnemonic,
  pub dst: Option<ParsedOperand>,
  pub src: Option<ParsedOperand>,
  pub undocumented: bool,
  pub raw_mnemonic: String,
}

#[derive(Debug)]
pub struct ParsedPattern {
  pub code: u16,
  pub arg1: Option<PatternArg>,
  pub arg2: Option<PatternArg>,
}


#[derive(Debug)]
pub enum PatternArg {
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
pub enum ParsedOperand {
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
  pub enum Mnemonic {
    ADC, ADD, AND, BIT, CALL, CCF, CP, CPD, CPDR, CPI, CPIR, CPL, DAA,
    DEC, DI, DJNZ, EI, EX, EXX, HALT, IM, IN, INC, IND, INDR, INI, INIR,
    JP, JR, LD, LDD, LDDR, LDI, LDIR, NEG, NOP, OR, OTDR, OTIR, OUT, OUTD,
    OUTI, POP, PUSH, RES, RET, RETI, RETN, RL, RLA, RLC, RLCA, RLD, RR, RRA,
    RRC, RRCA, RRD, RST, SBC, SCF, SET, SLA, SLL, SRA, SRL, SUB, XOR,
  }
}

custom_derive! {
  #[derive(Debug, EnumFromStr)]
  pub enum Register {
    B, C, D, E, H, L, A,
    I, R,
    IXh, IXl, IYh, IYl,
    BC, DE, HL, AF, SP, IX, IY,
    BC_, DE_, HL_, AF_,
  }
}

impl fmt::Display for Register {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    use self::Register::*;
    match self {
      &BC_ => write!(f, "BC'"),
      &DE_ => write!(f, "DE'"),
      &HL_ => write!(f, "HL'"),
      &AF_ => write!(f, "AF'"),
      r => write!(f, "{:?}", r),
    }
  }
}

custom_derive! {
  #[derive(Debug, EnumFromStr)]
  pub enum Condition {
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
    use self::PatternArg::*;

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

// TODO: Maybe this exists somewhere?
fn lift<A, B, E>(f: fn(A) -> Result<B, E>, a: Option<A>) -> Result<Option<B>, E> {
  a.map_or(Ok(None), |a| f(a).map(Some))
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
      arg1: lift(PatternArg::from_str, caps.get(2).as_ref().map(Match::as_str))?,
      arg2: lift(PatternArg::from_str, caps.get(3).as_ref().map(Match::as_str))?,
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

    use self::ParsedOperand::*;
    use self::Register::*;

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
      dst: lift(ParsedOperand::from_str, caps.get(2).as_ref().map(Match::as_str))?,
      src: lift(ParsedOperand::from_str, caps.get(3).as_ref().map(Match::as_str))?,
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
