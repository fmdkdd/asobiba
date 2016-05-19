// Entirely due to:
// http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html

extern crate libc;

extern {
  fn memset(s: *mut libc::c_void, c: libc::uint32_t, n: libc::size_t) -> *mut libc::c_void;
}

use std::mem;

const PAGE_SIZE: usize = 4096;

struct JitMemory {
  addr: *mut u8,
  size: usize,
}

impl JitMemory {
  fn new(num_pages: usize) -> JitMemory {
    let addr;
    let size = num_pages * PAGE_SIZE;

    unsafe {
      // Allocate memory
      let mut page = mem::uninitialized();
      libc::posix_memalign(&mut page, PAGE_SIZE, size);

      // Mark read-write for now, executable later
      libc::mprotect(page, size, libc::PROT_READ | libc::PROT_WRITE);

      // Fill with RET calls.  If the code falls anywhere in the region, the
      // function returns.
      memset(page, 0xC3, size);

      // From *c_void to *u8
      addr = mem::transmute(page);
    }

    JitMemory {
      size: size,
      addr: addr,
    }
  }

  fn make_exec(&self) {
    unsafe {
      let page = mem::transmute(self.addr);
      libc::mprotect(page, self.size, libc::PROT_READ | libc::PROT_EXEC);
    }
  }
}

// Return a function that will execute the given code
fn jit_code(code: &[u8]) -> (fn() -> i64) {
  // Enough to hold all the code
  let len = code.len();
  let pages = len / PAGE_SIZE + 1;

  let jit = JitMemory::new(pages);

  unsafe { std::ptr::copy_nonoverlapping(code.as_ptr(), jit.addr, len); }

  jit.make_exec();

  unsafe { mem::transmute(jit.addr) }
}

fn emulate(code: &[u8]) -> u64 {
  let mut pc = 0;
  let mut rcx : u64 = 0;
  let mut rax : u64 = 0;
  let mut zero : bool = false;

  'running: loop {
    let opcode = code[pc];
    pc += 1;

    match opcode {
      // wide reg
      0x48 => {
        let op = code[pc];
        pc += 1;

        match op {
          // MOV RCX
          0xB9 => {
            let mut v : u64 = 0;
            for b in 0..8 {
              v |= (code[pc] as u64) << (8 * b);
              pc += 1;
            }

            rcx = v;
          },

          // DEC RCX
          0xFF => {
            pc += 1;

            rcx = rcx.wrapping_sub(1);
          },

          // CMP RCX
          0x83 => {
            pc += 1;
            let v = code[pc] as u64;
            pc += 1;

            if rcx - v == 0 { zero = true }
            else { zero = false }
          },

          // MOV RAX
          0xC7 => {
            pc += 1;

            let mut v : u64 = 0;
            for b in 0..4 {
              v |= (code[pc] as u64) << (8 * b);
              pc += 1;
            }

            rax = v;
          },

          _ => panic!("Unknown op {:x}", op)
        }
      },

      // JNE
      0x75 => {
        pc += 1;

        if !zero {
          pc = 0xA;
        }
      },

      // RET
      0xC3 => break 'running,

      _ => panic!("Unknown opcode {:x}", opcode)
    }
  }

  rax
}


fn main() {
  // Return 3
  // let code = [0x48, 0xC7, 0xC0, 0x03, 0x00, 0x00, 0x00];

  // Return 3 after looping for a second or two
  let code = [ 0x48, 0xB9, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00, 0x00, // movabs rcx, 0xFF000000
               0x48, 0xFF, 0xC9,                                           // dec rcx
               0x48, 0x83, 0xF9, 0x00,                                     // cmp rcx, 0x0
               0x75, 0xF7,                                                 // jne a
               0x48, 0xC7, 0xC0, 0x03, 0x00, 0x00, 0x00,                   // mov rax, 0x3
               0xC3 ];                                                     // ret

  // let fun = jit_code(&code);
  // let r = fun();

  let r = emulate(&code);

  println!("{}", r);
}
