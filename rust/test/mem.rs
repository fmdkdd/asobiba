
struct Mem {
  ram: [u8; 0x1000],
}

impl Mem {
  fn read(&self, addr: usize) -> u8 {
    self.ram[addr]
  }

  fn write(&mut self, addr: usize, x: u8) {
    self.ram[addr] = x;
  }
}



struct Video {
  bus: Option<*mut Bus>,
}

struct CPU {
  bus: Option<*mut Bus>,
}

impl CPU {
  fn run(&mut self) {
    match self.bus {
      Some(b) => b.write(0, 42)
    }
  }
}

trait Bus {
  fn read(&self, addr: usize) -> u8;
  fn write(&mut self, addr: usize, x: u8);
}

struct Machine {
  cpu: CPU,
  vid: Video,
  mem: Mem,
}

impl Bus for Machine {
  fn read(&self, addr: usize) -> u8 {
    self.mem.read(addr)
  }

  fn write(&mut self, addr: usize, x: u8) {
    self.mem.write(addr, x);
  }
}

fn main() {
  let m = Machine {
    mem: Mem { ram: [0; 0x1000] },
    cpu: CPU { bus: None },
    vid: Video { bus: None },
  };

  m.cpu = CPU { bus: Some(&mut m) };
  m.vid = Video { bus: Some(&mut m) };

  println!("{}", m.read(0));

}
