
#[derive(Debug)]
struct Tile {
  a: u8,
  b: u8,
}

fn main() {

  {
    let mut ram = [1,2,3,4];

    let mut p = unsafe {
      let t = &mut ram[0];
      std::mem::transmute::<&mut u8, &mut Tile>(t)
    };

    // Nope:
    // let t = &ram as **const Tile;

    println!("{}", p.a);
    println!("{}", p.b);
    {
      println!("{:?}", ram);
    }

    p.a = 42;

    println!("{}", p.a);
    println!("{}", p.b);
    {
      println!("{:?}", ram);
    }
  }


  {
    let mut m = Mem::new();

    let tiles = [TilePtr {a: &mut m.ram[0], b: &mut m.ram[1]},
                 TilePtr {a: &mut m.ram[2], b: &mut m.ram[3]}];

    println!("{:?}", m);
    println!("{:?}", tiles);

    m.ram[0] = 42;

    println!("{:?}", m);

  }

}

#[derive(Debug)]
struct TilePtr<'a> {
  a: &'a mut u8,
  b: &'a mut u8,
}

#[derive(Debug)]
struct Mem {
  ram: [u8; 16],
}

impl Mem {
  pub fn new() -> Self {
    Mem {
      ram: [0u8; 16],
    }
  }

  pub fn read(&self, addr: usize) -> u8 {
    self.ram[addr]
  }

  pub fn write(&mut self, addr: usize, x: u8) {
    self.ram[addr] = x;
  }
}
