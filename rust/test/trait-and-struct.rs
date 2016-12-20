trait Bus {
  fn read(&self, addr: usize) -> u8;
  fn write(&mut self, addr: usize, w: u8);
}

struct Hardware {
  ram: [u8; 0x1000],
}

impl Hardware {
  fn new() -> Self {
    Hardware {
      ram: [0; 0x1000],
    }
  }
}

impl Bus for Hardware {
  fn read(&self, addr: usize) -> u8 {
    self.ram[addr]
  }

  fn write(&mut self, addr: usize, w: u8) {
    self.ram[addr] = w;
  }
}

// This works, but does not use the trait, only the impl

struct Cpu1 {
  bus: Hardware,
}

impl Cpu1 {
  fn new(bus: Hardware) -> Self {
    Cpu1 {
      bus: bus
    }
  }
}

// This works, but only when the trait is the last element

struct Cpu2 {
  a: u8,
  bus: Bus,
}

/// But then the constructor does not compile.  Self has no fixed size, even
/// though we explicitly pass Hardware, which do have a fixed size.

// impl Cpu2 {
//   fn new(bus: Hardware) -> Self {
//     Cpu2 {
//       a: 0,
//       bus: bus,
//     }
//   }
// }

/// Attempting to force the Sized trait does not work either:

// trait BusSized : Sized {
//   fn read(&self, addr: usize) -> u8;
// }

// impl BusSized for Hardware {
//   fn read(&self, addr: usize) -> u8 {
//     self.ram[addr]
//   }
// }

// struct Cpu3 {
//   a: u8,
//   bus: BusSized,  // <- error here
// }

/// This one works, and you can specify Hardware as an implementation.  But you
/// couple the constructor to a specific implementation.
struct Cpu4<B: Bus> {
  bus: B,
}

impl Cpu4<Hardware> {
  fn new(bus: Hardware) -> Self {
    Cpu4 {
      bus: bus,
    }
  }
}

/// Rather, to allow any struct implementing the Bus trait to be used in the
/// constructor, you should just use a trait bound all the way.
struct Cpu5<B: Bus> {
  bus: B,
}

impl<B> Cpu5<B> where B: Bus {
  fn new(bus: B) -> Self {
    Cpu5 {
      bus: bus,
    }
  }
}


struct Foo {}
impl Bus for Foo {
  fn read(&self, addr: usize) -> u8 {
    42
  }

  fn write(&mut self, addr: usize, w: u8) {
    ;
  }
}


// But... what if we have a /reference/ to a trait instead?  There shouldn't be
// any Sized trouble... only lifetimes nightmares.

struct Cpu6<'a> {
  bus: &'a mut Bus,
}

impl<'a> Cpu6<'a> {
  fn new(bus: &'a mut Bus) -> Self {
    Cpu6 {
      bus: bus,
    }
  }
}

// Well okay, that's tame.  The Bus should live as long as the Cpu.  Should be
// default but ok.

fn main() {
  {
    let h = Hardware::new();
    let mut c = Cpu1::new(h);
    c.bus.write(0, 42);
    println!("{}", c.bus.read(0));
  }

  // {
  //   let h = Hardware::new();
  //   let c2 = Cpu2::new();
  // }

  {
    let h = Hardware::new();
    let mut c = Cpu4::new(h);
    c.bus.write(0, 42);
    println!("{}", c.bus.read(0));

    // Nope:
    // let c4b = Cpu4::new(Foo{});
  }

  {
    let h = Hardware::new();
    let mut c = Cpu5::new(h);
    c.bus.write(0, 42);
    println!("{}", c.bus.read(0));

    let cb = Cpu5::new(Foo{});
  }

  {
    // Interestingly, that's the only case where h must be declared mutable.  In
    // the other cases, I guess h is swallowed by the Cpu struct and there's no
    // need for interior mutability.
    let mut h = Hardware::new();
    let mut c = Cpu6::new(&mut h);
    c.bus.write(0, 42);
    println!("{}", c.bus.read(0));

    let mut f = Foo{};
    let cb = Cpu6::new(&mut f);
  }

}
