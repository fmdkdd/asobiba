// Arithmetic language interpreter, bytecode compiler and native code compiler.
#![feature(test)]

extern crate test;

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

fn compile_machine(t: &Term) -> ? {
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

use test::Bencher;

#[bench]
fn bench_interp(b: &mut Bencher) {
  use Term::*;

  let p = Sub(&N(5), &Add(&N(1), &N(2)));
  b.iter(|| interp(&p))
}

#[bench]
fn bench_run(b: &mut Bencher) {
  use Term::*;

  let p = compile(&Sub(&N(5), &Add(&N(1), &N(2))));
  b.iter(|| run(&p))
}

fn main() {
  use Term::*;

  {
    let p = Add(&N(1), &N(2));
    println!("{}", interp(&p));
    println!("{:?}", compile(&p));
    println!("{}", run(&compile(&p)));
  }

  {
    let p = Sub(&N(5), &Add(&N(1), &N(2)));
    println!("{}", interp(&p));
    println!("{:?}", compile(&p));
    println!("{}", run(&compile(&p)));
  }

}
