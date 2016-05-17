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
  contents: *mut u8,
}

impl Index<usize> for JitMemory {
  type Output = u8;

  fn index(&self, _index: usize) -> &u8 {
    unsafe { &*self.contents.offset(_index as isize) }
  }
}

impl IndexMut<usize> for JitMemory {
  fn index_mut(&mut self, _index: usize) -> &mut u8 {
    unsafe { &mut *self.contents.offset(_index as isize) }
  }
}

impl JitMemory {
  fn new(num_pages: usize) -> JitMemory {
    let contents: *mut u8;
    let size = num_pages * PAGE_SIZE;

    unsafe {
      // Allocate memory
      let mut page : *mut libc::c_void = mem::uninitialized();
      libc::posix_memalign(&mut page, PAGE_SIZE, size);

      // Mark executable and read-write (quite unsafe)
      libc::mprotect(page, size,
                     libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE);

      // Fill with RET calls
      memset(page, 0xC3, size);

      contents = mem::transmute(page);
    }

    JitMemory { contents: contents }
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

  unsafe { mem::transmute(jit.contents) }
}

fn main() {
  let fun = run_jit();
  println!("{}", fun());
}
