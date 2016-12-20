use gb::utils::{from_u16, to_u16};

pub trait Bus {
  fn read(&self, addr: u16) -> u8;
  fn write(&mut self, addr: u16, w: u8);
  fn read_16le(&self, addr: u16) -> u16;
  fn write_16le(&mut self, addr: u16, ww: u16);
}

const RAM_SIZE : usize = 0x10000;

pub struct Hardware {
  pub ram: [u8; RAM_SIZE],
}

impl Hardware {
  pub fn new() -> Hardware {
    Hardware {
      ram: [0; RAM_SIZE],
    }
  }
}

const ASCII : [char; 256] = [
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',

  ' ', '!', '"', '$', '%', '&', '\'',
  '(', ')', '*', '+', ',', '-', '.',
  '/', '0', '1', '2', '3', '4', '5',
  '6', '7', '8', '9', ':', ';', '<',
  '=', '>', '?', '@', 'A', 'B', 'C',
  'D', 'E', 'F', 'G', 'H', 'I', 'J',
  'K', 'L', 'M', 'N', 'O', 'P', 'Q',
  'R', 'S', 'T', 'U', 'V', 'W', 'X',
  'Y', 'Z', '[', '\\', ']', '^', '_',
  '`', 'a', 'b', 'c', 'd', 'e', 'f',
  'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't',
  'u', 'v', 'w', 'x', 'y', 'z', '{',
  '}', '~', ' ',

  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ', ' ', ' ', ' ',
  ' ', ' ', ' ', ' ',

  ' ', '!', '"', '$', '%', '&', '\'',
  '(', ')', '*', '+', ',', '-', '.',
  '/', '0', '1', '2', '3', '4', '5',
  '6', '7', '8', '9', ':', ';', '<',
  '=', '>', '?', '@', 'A', 'B', 'C',
  'D', 'E', 'F', 'G', 'H', 'I', 'J',
  'K', 'L', 'M', 'N', 'O', 'P', 'Q',
  'R', 'S', 'T', 'U', 'V', 'W', 'X',
  'Y', 'Z', '[', '\\', ']', '^', '_',
  '`', 'a', 'b', 'c', 'd', 'e', 'f',
  'g', 'h', 'i', 'j', 'k', 'l', 'm',
  'n', 'o', 'p', 'q', 'r', 's', 't',
  'u', 'v', 'w', 'x', 'y', 'z', '{',
  '}', '~', ' ',

  ' ', ' ', ' ', ' '
];

impl Bus for Hardware {
  fn read(&self, addr: u16) -> u8 {
    self.ram[addr as usize]

  }

  fn write(&mut self, addr: u16, w: u8) {
    if cfg!(feature = "debug") {
      match addr {
        0xFF01 => println!("{:x} {}", w, ASCII[w as usize]),
        0xFF50 => println!("bingo"),
        _ => {},
      }
    }

    self.ram[addr as usize] = w;
  }

  fn read_16le(&self, addr: u16) -> u16 {
    let l = self.read(addr);
    let h = self.read((addr.wrapping_add(1)));
    to_u16(h, l)
  }

  fn write_16le(&mut self, addr: u16, ww: u16) {
    let (h, l) = from_u16(ww);
    self.write(addr, l);
    self.write(addr.wrapping_add(1), h);
  }
}
