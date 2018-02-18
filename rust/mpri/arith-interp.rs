// Arithmetic language interpreter, bytecode compiler and native code compiler.
#![feature(test)]
#![feature(libc)]

extern crate test;
extern crate libc;

extern {
  fn memset(s: *mut libc::c_void, c: libc::uint32_t, n: libc::size_t) -> *mut libc::c_void;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Term interpreter

#[derive(PartialEq, Eq, Clone, Debug)]
enum Term<'a> {
  N(usize),
  Add(&'a Term<'a>, &'a Term<'a>),
  Sub(&'a Term<'a>, &'a Term<'a>),
}

type Value = usize;

fn interp<'a>(t: &Term<'a>) -> Value {
  match *t {
    Term::N(n)      => n,
    Term::Add(a, b) => interp(a) + interp(b),
    Term::Sub(a, b) => interp(a) - interp(b),
  }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Op {
  N(usize),
  Add,
  Sub,
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Bytecode compiler and interpreter

type Bytecode = Vec<Op>;

fn compile<'a>(t: &Term<'a>) -> Bytecode {
  let mut bc = Vec::new();

  match *t {
    Term::N(n)      => bc.push(Op::N(n)),
    Term::Add(a, b) => {
      bc.append(&mut compile(a));
      bc.append(&mut compile(b));
      bc.push(Op::Add)
    },
    Term::Sub(a, b) => {
      bc.append(&mut compile(a));
      bc.append(&mut compile(b));
      bc.push(Op::Sub)
    },
  }

  bc
}

fn run(b: &Bytecode) -> Value {
  let mut stack = Vec::new();

  for op in b {
    match *op {
      Op::N(n) => stack.push(n),
      Op::Add  => {
        let b = stack.pop().unwrap();
        let a = stack.pop().unwrap();
        stack.push(a + b);
      },
      Op::Sub  => {
        let b = stack.pop().unwrap();
        let a = stack.pop().unwrap();
        stack.push(a - b);
      }
    }
  }

  stack.pop().unwrap()
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Native code compiler

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

fn compile_machine(t: &Term) -> Vec<u8> {
  let mut code = Vec::new();

  for op in compile(t) {
    match op {
      Op::N(n) => {
        // push n
        code.push(0x6A);
        code.push(n as u8);
      },
      Op::Add => {
        // pop rax
        code.push(0x58);
        // add [rsp], rax
        code.append(&mut vec![0x48, 0x01, 0x04, 0x24]);
      },
      Op::Sub => {
        // pop rax
        code.push(0x58);
        // sub [rsp], rax
        code.append(&mut vec![0x48, 0x29, 0x04, 0x24]);
      },
    }
  }

  code.push(0x58);              // pop rax
  code.push(0xC3);              // ret

  code
}

fn print_machine_code(code: &[u8]) -> String {
  code.iter().map(|x| format!("{:02x}", x)).collect::<Vec<String>>().join(" ")
}

use test::Bencher;

#[bench]
fn bench_interp(b: &mut Bencher) {
  use Term::*;

  let p = Sub(&N(5), &Add(&N(1), &N(2)));
  b.iter(|| interp(&p))
}

#[bench]
fn bench_vm(b: &mut Bencher) {
  use Term::*;

  let p = compile(&Sub(&N(5), &Add(&N(1), &N(2))));
  b.iter(|| run(&p))
}

#[bench]
fn bench_jit(b: &mut Bencher) {
  use Term::*;

  let fun = jit_code(&compile_machine(&Sub(&N(5), &Add(&N(1), &N(2)))));
  b.iter(|| fun())
}

fn main() {
  use Term::*;

  {
    let p = Add(&N(1), &N(2));
    println!("{}", interp(&p));
    println!("{:?}", compile(&p));
    println!("{}", run(&compile(&p)));
    println!("{}", print_machine_code(&compile_machine(&p)));
    println!("{}", jit_code(&compile_machine(&p))());
  }

  {
    let p = Sub(&N(5), &Add(&N(1), &N(2)));
    println!("{}", interp(&p));
    println!("{:?}", compile(&p));
    println!("{}", run(&compile(&p)));
    println!("{}", print_machine_code(&compile_machine(&p)));
    println!("{}", jit_code(&compile_machine(&p))());
  }

}
