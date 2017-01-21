use std::ops::{Index, IndexMut};

struct Cpu {


}


struct Register(pub u8);

enum R {
  A, F
}

enum RR {
  AF,
}

struct Registers {
  a: u8,
  f: u8,
}

impl Registers {

  pub fn new() -> Self {
    Registers {
      a: 0,
      f: 0,
    }
  }

  fn get(&self, r: R) -> u8 {
    match r {
      R::A => self.a,
      R::F => self.f,
    }
  }

  fn set(&mut self, r: R, x: u8) {
    match r {
      R::A => self.a = x,
      R::F => self.f = x,
    }
  }

  fn get16(&self, rr: RR) -> u16 {
    match rr {
      RR::AF => ((self.a as u16) << 8) | (self.f as u16),
    }
  }

  fn set16(&mut self, rr: RR, x: u16) {
    match rr {
      RR::AF => {
        self.a = (x >> 8) as u8;
        self.f = x as u8;
      },
    }
  }

  fn f(N) ->  {

  }
}

fn main() {
  let r = Register(0);

  println!("{:x}", r.0);

  let mut rr = Registers::new();
  use self::R::{A, F};
  use self::RR::AF;

  rr.a = 0xFE;

  println!("{:x}", rr.a);
  println!("{:x}", rr.get16(AF));

  rr.set16(AF, 0x1234);
  println!("{:x}", rr.a);
  println!("{:x}", rr.get16(AF));

  rr.a += 1;
  println!("{:x}", rr.a);
  println!("{:x}", rr.get16(AF));

}
