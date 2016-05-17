// Entirely due to:
// http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html

extern crate libc;

extern {
  fn memset(s: *mut libc::c_void, c: libc::uint32_t, n: libc::size_t) -> *mut libc::c_void;
}

use std::mem;
use std::ops::{Index, IndexMut};

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

impl Index<usize> for JitMemory {
  type Output = u8;

  fn index(&self, _index: usize) -> &u8 {
    unsafe { &*self.addr.offset(_index as isize) }
  }
}

impl IndexMut<usize> for JitMemory {
  fn index_mut(&mut self, _index: usize) -> &mut u8 {
    unsafe { &mut *self.addr.offset(_index as isize) }
  }
}

fn run_jit() -> (fn() -> i64) {
  let mut jit = JitMemory::new(1);

  jit[0] = 0x48;
  jit[1] = 0xC7;
  jit[2] = 0xC0;
  jit[3] = 0x03;
  jit[4] = 0x00;
  jit[5] = 0x00;
  jit[6] = 0x00;

  jit.make_exec();

  unsafe { mem::transmute(jit.addr) }
}

fn main() {
  let fun = run_jit();
  println!("{}", fun());
}
