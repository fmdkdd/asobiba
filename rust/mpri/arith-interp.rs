// Arithmetic language interpreter, bytecode compiler and native code compiler.
#![feature(test)]
#![feature(libc)]

extern crate test;
extern crate libc;

extern {
  fn memset(s: *mut libc::c_void, c: libc::uint32_t, n: libc::size_t) -> *mut libc::c_void;
  fn rand() -> isize;
  fn srand(seed: usize);
  fn time(tloc: *const usize) -> usize;
}

use std::io;
use std::str::FromStr;
use std::fmt::Display;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parser

#[derive(PartialEq, Eq, Clone, Debug)]
enum Term {
  N(usize),
  Add(Box<Term>, Box<Term>),
  Sub(Box<Term>, Box<Term>),
}

impl Display for Term {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result<> {
    match self {
      &Term::N(n) => write!(f, "{}", n),
      &Term::Add(ref a, ref b) => write!(f, "(+ {} {})", a, b),
      &Term::Sub(ref a, ref b) => write!(f, "(- {} {})", a, b),
    }
  }
}

#[derive(Clone, Debug)]
enum ParseError {
  Unexpected(String),
  ParseNumError(String),
}

struct TokenStream<'a> {
  tokens: Vec<&'a str>,
  index: usize,
}

impl<'a> TokenStream<'a> {
  fn new(tokens: Vec<&'a str>) -> Self {
    TokenStream {
      tokens: tokens,
      index: 0,
    }
  }

  fn advance(&mut self) -> &str {
    let t = self.tokens[self.index];
    self.index += 1;
    t
  }

  fn peek(&self) -> &str {
    self.tokens[self.index]
  }

  fn expect(&mut self, s: &str) -> Result<(), ParseError> {
    let t = self.advance();
    if t == s {
      Ok(())
    } else {
      Err(ParseError::Unexpected(format!("Expected {}, got {}", s, t)))
    }
  }
}

fn parse(input: &str) -> Result<Term, ParseError> {
  let t1 = input.replace("(", " ( ");
  let t2 = t1.replace(")", " ) ");
  let t3 = t2.split(" ")
    .filter(|s| s.len() > 0)
    .collect();
  parse_expr(&mut TokenStream::new(t3))
}

fn parse_expr(t: &mut TokenStream) -> Result<Term, ParseError> {
  if t.peek() == "(" {
    t.expect("(")?;
    let p = parse_op(t);
    t.expect(")")?;
    p
  } else {
    parse_num(t)
  }
}

fn parse_op(t: &mut TokenStream) -> Result<Term, ParseError> {
  // Ahemmm..
  match t.advance().to_string().as_str() {
    "+" => {
      let a = parse_expr(t)?;
      let b = parse_expr(t)?;
      Ok(Term::Add(Box::new(a), Box::new(b)))
    },
    "-" => {
      let a = parse_expr(t)?;
      let b = parse_expr(t)?;
      Ok(Term::Sub(Box::new(a), Box::new(b)))
    },
    x => Err(ParseError::Unexpected(format!("Expected '+' or '-', got {}", x))),
  }
}

fn parse_num(t: &mut TokenStream) -> Result<Term, ParseError> {
  let n = t.advance();
  match usize::from_str(n) {
    Ok(n) => Ok(Term::N(n)),
    Err(_) => Err(ParseError::ParseNumError(format!("Can't parse num {}", n))),
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Generate random somewhat long term

fn gen(depth: usize) -> Term {
  let n = unsafe {
    rand()
  };

  let d = depth - 1;

  match n % 4 {
    0 | 1 => Term::N(d),
    2 => Term::Add(Box::new(gen(d)), Box::new(gen(d))),
    3 => Term::Sub(Box::new(gen(d)), Box::new(gen(d))),
    _ => unreachable!(),
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Term interpreter

type Value = usize;

fn interp<'a>(t: &Term) -> Value {
  match t {
    &Term::N(n)      => n,
    &Term::Add(ref a, ref b) => interp(a).wrapping_add(interp(b)),
    &Term::Sub(ref a, ref b) => interp(a).wrapping_sub(interp(b)),
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Bytecode compiler and interpreter

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Op {
  N(usize),
  Add,
  Sub,
}

type Bytecode = Vec<Op>;

fn compile<'a>(t: &Term) -> Bytecode {
  let mut bc = Vec::new();

  match t {
    &Term::N(n)      => bc.push(Op::N(n)),
    &Term::Add(ref a, ref b) => {
      bc.append(&mut compile(a));
      bc.append(&mut compile(b));
      bc.push(Op::Add)
    },
    &Term::Sub(ref a, ref b) => {
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
        stack.push(a.wrapping_add(b));
      },
      Op::Sub  => {
        let b = stack.pop().unwrap();
        let a = stack.pop().unwrap();
        stack.push(a.wrapping_sub(b));
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

const PROG : &'static str = r#"(- (+ (+ 996 (+ (+ (+ (+ (- (- (- 989 (+ 988 (- 987 (+ 986 986)))) (- (+ (+ (- (+ (- 984 984) (- (- (+ (- (+ 980 (+ 979 (+ 978 (+ 977 977)))) (+ (- 979 979) 980)) 982) 983) (+ (+ 982 982) 983))) (- 985 (- (+ 983 (- 982 982)) (+ 983 (- (+ (- (- 979 (- 978 978)) (- (- 978 978) (- 978 978))) 981) (- (- 980 980) (- 980 980))))))) (+ (- (+ (+ 983 (+ 982 (+ 981 (+ 980 980)))) (- 983 (+ 982 982))) 985) (+ 985 985))) (- 987 987)) (+ 988 (- (+ 986 986) (- (+ (- 984 984) 985) 986))))) 991) (+ 991 (- (- (+ (+ 987 987) (- 987 987)) 989) 990))) (+ 992 (+ 991 (+ (+ (- 988 (+ (- 986 986) (+ (+ 985 (- (+ (+ 982 (- 981 981)) 983) (- 983 983))) (- 985 (- 984 984))))) 989) 990)))) (- 993 (- (+ 991 991) 992))) (- 994 994))) (+ (- (- 994 994) 995) (+ 995 (+ (- (+ 992 992) (- 992 992)) (+ 993 (- 992 (+ (+ 990 990) 991))))))) (+ (+ 996 996) (- (+ (- (+ 993 993) (- (+ (- (- 990 (+ 989 (+ (- 987 (+ 986 (- (- (- (- 982 (+ 981 (- (+ (- 978 (- 977 977)) (+ 978 (+ (- 976 976) (+ (- 975 (+ (+ (+ 972 972) (- (- 971 (+ (+ 969 (+ 968 (- (- (- 965 (+ 964 964)) (+ 965 (- (+ (+ (+ (- (- 959 (+ (- (- 956 956) (- 956 956)) (+ (- (- 955 955) 956) 957))) (- 959 959)) 961) (+ 961 961)) 963) 964))) 967))) (- (+ (- (+ 966 (- 965 965)) (+ 966 (+ (+ (+ (+ 962 962) 963) 964) 965))) (+ (+ 966 (+ (+ 964 964) (+ (+ 963 (+ (- 961 961) (+ (+ 960 960) (+ (+ 959 (- 958 (+ (+ (+ (+ (- 953 953) 954) (+ (+ 953 953) 954)) (+ 955 (- 954 (+ (+ 952 (+ (+ (+ 949 949) (- (- (+ (- (+ (+ 944 944) 945) 946) 947) 948) 949)) 951)) (- (+ (+ 950 (- 949 949)) (- (+ 949 949) (+ (+ (- 947 (- (- (- (+ 943 (- 942 (- 941 941))) 944) 945) 946)) 948) 949))) (+ (- (- 949 (- (+ 947 947) 948)) (- 949 (+ 948 (- (+ 946 (+ 945 945)) 947)))) (- 950 950))))))) (+ 956 (+ (+ 954 (- (- 952 952) (+ 952 952))) (+ (- (- (- 951 (+ 950 (- 949 (- 948 948)))) (+ (+ (+ (+ 948 (+ (- 946 (- 945 (+ 944 944))) 947)) 949) 950) 951)) (+ (- (+ 950 (+ 949 949)) 951) 952)) 954)))))) 960)))) 964))) 967)) (+ (+ 967 967) 968)))) 972)) (- 973 973))) (+ (+ 974 (+ (+ 972 (- 971 971)) 973)) (- (- (+ 972 972) 973) 974)))))) 980))) 983) (+ (+ (+ 981 981) (+ 981 981)) (- (- (+ 980 980) 981) 982))) (- (+ (+ (- (+ 980 980) 981) (- (- (+ 979 (- 978 978)) (- (+ (- 977 (+ 976 (- (+ 974 (- (+ (- 971 (- (- 969 (- (+ (- (+ 965 (- (+ (+ (- 961 961) (- 961 961)) 963) 964)) (+ 965 965)) 967) 968)) 970)) (- 971 971)) 973)) 975))) 978) 979)) 981)) 983) (- (+ 982 982) (- 982 982)))))) 988))) (- 990 990)) (+ (+ (+ (+ (+ 987 (+ (- 985 (+ 984 984)) 986)) 988) 989) 990) 991)) 993)) 995) 996)))"#;

#[bench]
fn bench_interp(b: &mut Bencher) {
  let p = parse(PROG).unwrap();
  b.iter(|| interp(&p))
}

#[bench]
fn bench_vm(b: &mut Bencher) {
  let p = compile(&parse(PROG).unwrap());
  b.iter(|| run(&p))
}

#[bench]
fn bench_jit(b: &mut Bencher) {
  let fun = jit_code(&compile_machine(&parse(PROG).unwrap()));
  b.iter(|| fun())
}

#[test]
fn test() {
  use Term::*;

  let p = parse("(- 5 (+ 1 2))").unwrap();
  assert_eq!(Sub(Box::new(N(5)), Box::new(Add(Box::new(N(1)), Box::new(N(2))))), p);
  assert_eq!(2, interp(&p));
  assert_eq!(vec![Op::N(5), Op::N(1), Op::N(2), Op::Add, Op::Sub], compile(&p));
  assert_eq!(2, run(&compile(&p)));
  assert_eq!(vec![0x6a, 0x05, 0x6a, 0x01, 0x6a, 0x02, 0x58, 0x48, 0x01,
                  0x04, 0x24, 0x58, 0x48, 0x29, 0x04, 0x24, 0x58, 0xc3],
             compile_machine(&p));
  assert_eq!(2, jit_code(&compile_machine(&p))());
}

fn main() {
  unsafe {
    srand(time(std::ptr::null()));
  }

  let program = gen(1000);
  println!("{}", program);

  // let mut input = String::new();
  // io::stdin().read_line(&mut input).unwrap();
  // let program = parse(&input).unwrap();
  // println!("{}", jit_code(&compile_machine(&program))());
}
