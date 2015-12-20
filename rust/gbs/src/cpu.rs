const RAM_LENGTH : usize = 0x10000;

const Z_FLAG : u8 = 0x80;
const N_FLAG : u8 = 0x40;
const H_FLAG : u8 = 0x20;
const C_FLAG : u8 = 0x10;

pub struct Cpu {
  pub a: u8,
  b: u8,
  c: u8,
  d: u8,
  e: u8,
  f: u8,
  h: u8,
  l: u8,
  pub sp: u16,
  pub pc: u16,
  ram: [u8; RAM_LENGTH],

  cycles: u64,
}

impl Cpu {

  pub fn new() -> Cpu {
    Cpu {
      a: 0,
      b: 0,
      c: 0,
      d: 0,
      e: 0,
      f: 0,
      h: 0,
      l: 0,
      sp: 0,
      pc: 0,
      ram: [0; RAM_LENGTH],
      cycles: 0,
    }
  }

  pub fn reset(&mut self) {
    // TODO
  }

  pub fn load_rom(&mut self, rom: &Vec<u8>, offset: usize) {
    // Copy ROM into RAM banks 0 and 1, stopping at 0x7fff, or when ROM is
    // empty.
    let mut ra = offset;
    let ra_max = 0x8000;
    let mut ro = 0;
    let ro_max = rom.len();
    while ro < ro_max && ra < ra_max {
      self.ram[ra] = rom[ro];
      ra += 1;
      ro += 1;
    }
  }

  fn read_pc(&mut self) -> u8 {
    let ret = self.read(self.pc);
    self.pc += 1;
    ret
  }

  fn read(&self, addr: u16) -> u8 {
    self.ram[addr as usize]
  }

  fn write(&mut self, addr: u16, x: u8) {
    self.ram[addr as usize] = x;
  }

  pub fn run(&mut self, cycles: u64) {
    macro_rules! to_u16 {
      ($h:expr, $l:expr) => (($h as u16) << 8 | ($l as u16));
    }

    macro_rules! from_u16 {
      ($hl:expr) => ( (($hl >> 8) as u8, $hl as u8) )
    }

    macro_rules! nop {
      () => ({ self.cycles += 4; });
    }

    macro_rules! ld {
      // Order of these is important.  Match on first argument,
      // then discriminate on second.

      // LD (FF00+n),A
      ((0xFF00 + n), a) => ({
        let n = self.read_pc();
        let addr = 0xFF00 | (n as u16);
        let v = self.a;
        self.write(addr, v);
        self.cycles += 12;
      });

      // LD (FF00+C),A
      ((0xFF00 + c), a) => ({
        let addr = 0xFF00 | (self.c as u16);
        let v = self.a;
        self.write(addr, v);
        self.cycles += 12;
      });

      // LD (nn),A
      ((n n), a) => ({
        let l = self.read_pc();
        let h = self.read_pc();
        let addr = to_u16!(h, l);
        let v = self.a;
        self.write(addr, v);
        self.cycles += 16;
      });

      // LD (HL),n
      ((h l), n) => ({
        let n = self.read_pc();
        let addr = to_u16!(self.h, self.l);
        self.write(addr, n);
        self.cycles += 12;
      });

      // LD (HL),r
      // LD (BC),A
      // LD (DE),A
      (($rh:ident $rl:ident), $r2:ident) => ({
        let addr = to_u16!(self.$rh, self.$rl);
        let v = self.$r2;
        self.write(addr, v);
        self.cycles += 8;
      });

      // LD A,(FF00+n)
      (a, (0xFF00 + n)) => ({
        let n = self.read_pc();
        let addr = 0xFF00 | (n as u16);
        self.a = self.read(addr);
        self.cycles += 12;
      });

      // LD A,(FF00+C)
      (a, (0xFF00 + c)) => ({
        let addr = 0xFF00 | (self.c as u16);
        self.a = self.read(addr);
        self.cycles += 8;
      });

      // LD A,(nn)
      ($r1:ident, (n n)) => ({
        let l = self.read_pc();
        let h = self.read_pc();
        let addr = to_u16!(h, l);
        self.$r1 = self.read(addr);
        self.cycles += 16;
      });

      // LD r,(HL)
      // LD A,(BC)
      // LD A,(DE)
      ($r1:ident, ($rh:ident $rl:ident)) => ({
        let addr = to_u16!(self.$rh, self.$rl);
        self.$r1 = self.read(addr);
        self.cycles += 8;
      });

      // LD SP,nn
      (sp, nn) => ({
        let l = self.read_pc();
        let h = self.read_pc();
        self.sp = to_u16!(h, l);
        self.cycles += 12;
      });

      // LD r,n
      ($r1:ident, n) => ({
        let n = self.read_pc();
        self.$r1 = n;
        self.cycles += 8;
      });

      // LD r,r
      ($r1:ident, $r2:ident) => ({
        self.$r1 = self.$r2;
        self.cycles += 4;
      });

      // LD SP,HL
      (sp, h l) => ({
        self.sp = to_u16!(self.h, self.l);
        self.cycles += 8;
      });

      // LD rr,nn
      ($rh:ident $rl:ident, nn) => ({
        self.$rl = self.read_pc();
        self.$rh = self.read_pc();
        self.cycles += 12;
      });
    }

    macro_rules! ldd {
      // LDD (HL),A
      ((h l), a) => ({
        let mut addr = to_u16!(self.h, self.l);
        let v = self.a;
        self.write(addr, v);
        addr -= 1;              // TODO: should this wrap?
        let (h, l) = from_u16!(addr);
        self.h = h;
        self.l = l;
        self.cycles += 8;
      });

      // LDD A,(HL)
      (a, (h l)) => ({
        let mut addr = to_u16!(self.h, self.l);
        self.a = self.read(addr);
        addr -= 1;              // TODO: should this wrap?
        let (h, l) = from_u16!(addr);
        self.h = h;
        self.l = l;
        self.cycles += 8;
      })
    }

    macro_rules! ldi {
      // LDD (HL),A
      ((h l), a) => ({
        let mut addr = to_u16!(self.h, self.l);
        let v = self.a;
        self.write(addr, v);
        addr += 1;              // TODO: should this wrap?
        let (h, l) = from_u16!(addr);
        self.h = h;
        self.l = l;
        self.cycles += 8;
      });

      // LDD A,(HL)
      (a, (h l)) => ({
        let mut addr = to_u16!(self.h, self.l);
        self.a = self.read(addr);
        addr += 1;              // TODO: should this wrap?
        let (h, l) = from_u16!(addr);
        self.h = h;
        self.l = l;
        self.cycles += 8;
      })
    }

    macro_rules! inc {
      // INC (HL)
      ((h l)) => ({
        let addr = to_u16!(self.h, self.l);
        let v = self.read(addr);
        let r = v.wrapping_add(1);
        self.write(addr, r);
        flags!(z0h-, v, r);
        self.cycles += 12;
      });

      // INC sp
      (sp) => ({
        self.sp += 1;           // TODO: should this wrap?
        self.cycles += 8;
      });

      // INC r
      ($r:ident) => ({
        let v = self.$r;
        self.$r = v.wrapping_add(1);
        flags!(z0h-, v, self.$r);
        self.cycles += 4;
      });

      // INC rr
      ($rh:ident $rl:ident) => ({
        let mut rr = to_u16!(self.$rh, self.$rl);
        rr += 1;                // TODO: should this wrap?
        let (h, l) = from_u16!(rr);
        self.$rh = h;
        self.$rl = l;
        self.cycles += 8;
      });
    }

    macro_rules! dec {
      // DEC (HL)
      ((h l)) => ({
        let addr = to_u16!(self.h, self.l);
        let v = self.read(addr);
        let r = v.wrapping_sub(1);
        self.write(addr, r);
        flags!(z1h-, v, r);
        self.cycles += 12;
      });

      // DEC sp
      (sp) => ({
        self.sp -= 1;           // TODO: should this wrap?
        self.cycles += 4;
      });

      // DEC r
      ($r:ident) => ({
        let v = self.$r;
        self.$r = v.wrapping_sub(1);
        flags!(z1h-, v, self.$r);
        self.cycles += 4;
      });

      // DEC rr
      ($rh:ident $rl:ident) => ({
        let mut rr = to_u16!(self.$rh, self.$rl);
        rr -= 1;                // TODO: should this wrap?
        let (h, l) = from_u16!(rr);
        self.$rh = h;
        self.$rl = l;
        self.cycles += 4;
      });
    }

    macro_rules! add {
      ($r:ident) => ({
        let r = self.a.wrapping_add(self.$r);
        // TODO: flags
        self.a = r;
      });
    }

    macro_rules! flags {
      // First argument stands for znhc flags.
      //   z: set if $r is 0
      //   n: 0 or 1
      //   h: set if nibble overflow
      //   c: ?
      // $v: value before operation
      // $r: value after operation

      (z0h-, $v:expr, $r:expr) => ({
        self.f =
          ((($r == 0) as u8) << 7)
          | (((($v & 0xF) == 0xF) as u8) << 5)
          | (self.f & C_FLAG);
      });

      (z1h-, $v:expr, $r:expr) => ({
        self.f =
          ((($r == 0) as u8) << 7)
          | ((($v == 0) as u8) << 5)
          | N_FLAG
          | (self.f & C_FLAG);
      });
    }


    self.cycles = 0;

    while self.cycles < cycles {
      let opcode = self.read_pc();
      match opcode {

        0x20 => unimplemented!(), // JR NZ
        0x21 => ld!(h l, nn),
        0x28 => unimplemented!(), // JR Z

        0xF0 => ld!(a, (0xFF00 + n)),
        0xF2 => ld!(a, (0xFF00 + c)),
        0x0A => ld!(a, (b c)),
        0x3A => ldd!(a, (h l)),
        0x1A => ld!(a, (d e)),
        0x2A => ldi!(a, (h l)),
        0xFA => ld!(a, (n n)),

        // CMP

        // LD r,(HL)
        0x46 => ld!(b, (h l)),
        0x4E => ld!(c, (h l)),
        0x56 => ld!(d, (h l)),
        0x5E => ld!(e, (h l)),
        0x66 => ld!(h, (h l)),
        0x6E => ld!(l, (h l)),
        0x7E => ld!(a, (h l)),

        // CNZ

        // CALL

        // RNZ

        // RET

        // NOP
        // LD B,B
        // LD C,C
        // LD D,D
        // LD E,E
        // LD H,H
        // LD L,L
        // LD A,A
        0x00 => nop!(),
        0x40 => nop!(),
        0x49 => nop!(),
        0x52 => nop!(),
        0x5B => nop!(),
        0x64 => nop!(),
        0x6D => nop!(),
        0x7F => nop!(),

        // CB

        // SHIFT

        // LD (HL),r
        0x70 => ld!((h l), b),
        0x71 => ld!((h l), c),
        0x72 => ld!((h l), d),
        0x73 => ld!((h l), e),
        0x74 => ld!((h l), h),
        0x75 => ld!((h l), l),
        0x77 => ld!((h l), a),

        // LD r,r
        0x41 => ld!(b, c),
        0x42 => ld!(b, d),
        0x43 => ld!(b, e),
        0x44 => ld!(b, h),
        0x45 => ld!(b, l),
        0x47 => ld!(b, a),

        0x48 => ld!(c, b),
        0x4A => ld!(c, d),
        0x4B => ld!(c, e),
        0x4C => ld!(c, h),
        0x4D => ld!(c, l),
        0x4F => ld!(c, a),

        0x50 => ld!(d, b),
        0x51 => ld!(d, c),
        0x53 => ld!(d, e),
        0x54 => ld!(d, h),
        0x55 => ld!(d, l),
        0x57 => ld!(d, a),

        0x58 => ld!(e, b),
        0x59 => ld!(e, c),
        0x5A => ld!(e, d),
        0x5C => ld!(e, h),
        0x5D => ld!(e, l),
        0x5F => ld!(e, a),

        0x60 => ld!(h, b),
        0x61 => ld!(h, c),
        0x62 => ld!(h, d),
        0x63 => ld!(h, e),
        0x65 => ld!(h, l),
        0x67 => ld!(h, a),

        0x68 => ld!(l, b),
        0x69 => ld!(l, c),
        0x6A => ld!(l, d),
        0x6B => ld!(l, e),
        0x6C => ld!(l, h),
        0x6F => ld!(l, a),

        0x78 => ld!(a, b),
        0x79 => ld!(a, c),
        0x7A => ld!(a, d),
        0x7B => ld!(a, e),
        0x7C => ld!(a, h),
        0x7D => ld!(a, l),

        // LD 16
        0xF9 => ld!(sp, h l),
        0x31 => ld!(sp, nn),
        0x01 => ld!(b c, nn),
        0x11 => ld!(d e, nn),


        0xE0 => ld!((0xFF00 + n), a),
        0xE2 => ld!((0xFF00 + c), a),
        0x32 => ldd!((h l), a),
        0x02 => ld!((b c), a),
        0x12 => ld!((d e), a),
        0x22 => ldi!((h l), a),
        0xEA => ld!((n n), a),
        0x06 => ld!(b, (n n)),
        0x0E => ld!(c, (n n)),
        0x16 => ld!(d, (n n)),
        0x1E => ld!(e, (n n)),
        0x26 => ld!(h, (n n)),
        0x2E => ld!(l, (n n)),

        0x36 => ld!((h l), n),
        0x3E => ld!(a, n),

        0x03 => inc!(b c),
        0x13 => inc!(d e),
        0x23 => inc!(h l),
        0x33 => inc!(sp),

        0x0B => dec!(b c),
        0x1B => dec!(d e),
        0x2B => dec!(h l),
        0x3B => dec!(sp),

        0x34 => inc!((h l)),

        0x04 => inc!(b),
        0x0C => inc!(c),
        0x14 => inc!(d),
        0x1C => inc!(e),
        0x24 => inc!(h),
        0x2C => inc!(l),
        0x3C => inc!(a),

        0x35 => dec!((h l)),

        0x05 => dec!(b),
        0x0D => dec!(c),
        0x15 => dec!(d),
        0x1D => dec!(e),
        0x25 => dec!(h),
        0x2D => dec!(l),
        0x3D => dec!(a),

        // TODO: add 16bit

        0x80 => add!(b),
        0x81 => add!(c),
        0x82 => add!(d),
        0x83 => add!(e),
        0x84 => add!(h),
        0x85 => add!(l),
        0x87 => add!(a),

        _ => panic!(format!("Unknown opcode 0x{:x}", opcode))
      }
    }
  }
}
