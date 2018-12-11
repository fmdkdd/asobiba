use std::fmt::Display;

use crate::parse::{AST, Decl, Expr, Prim1, Prim2, Prog};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compiler
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Number each node of the AST
fn number<T>(ast: AST<T>) -> AST<(usize, T)> {
  AST {
    root: tag_prog(ast.root, 1).0,
    symbols: ast.symbols.clone(),
  }
}

fn tag_prog<T>(prog: Prog<T>, seed: usize) -> (Prog<(usize, T)>, usize) {
  let mut decls = Vec::new();

  let mut s = seed;
  for d in prog.decls {
    let (d_t, s1) = tag_decl(d, s);
    s = s1;
    decls.push(d_t);
  }

  let (body, s1) = tag_expr(prog.body, s);
  s = s1;

  (Prog { decls, body }, s)
}

fn tag_decl<T>(decl: Decl<T>, seed: usize) -> (Decl<(usize, T)>, usize) {

  let (body, seed) = tag_expr(decl.body, seed);

  (Decl {
    name: decl.name,
    args: decl.args,
    body
  }, seed)
}

fn tag_expr<T>(expr: Expr<T>, seed: usize) -> (Expr<(usize, T)>, usize) {
  use self::Expr::*;

  match expr {
    Number(n, t) => (Number(n, (seed, t)), seed+1),
    Id(s, t) => (Id(s, (seed, t)), seed+1),
    Bool(b, t) => (Bool(b, (seed, t)), seed+1),
    Expr::Prim1(op, exp, t) => {
      let (e, seed) = tag_expr(*exp, seed);
      (Prim1(op, Box::new(e), (seed, t)), seed+1)
    }
    Prim2(op, left, right, t) => {
      let (l, seed) = tag_expr(*left, seed);
      let (r, seed) = tag_expr(*right, seed);
      (Prim2(op, Box::new(l), Box::new(r), (seed, t)), seed+1)
    }
    Apply(id, exps, t) => {
      let mut exps_t = Vec::new();
      let mut seed = seed;
      for e in exps {
        let (e_t, s1) = tag_expr(e, seed);
        exps_t.push(e_t);
        seed = s1;
      }
      (Apply(id, exps_t, (seed, t)), seed+1)
    }
    If(cond, then, els, t) => {
      let (c, seed) = tag_expr(*cond, seed);
      let (th, seed) = tag_expr(*then, seed);
      let (el, seed) = tag_expr(*els, seed);
      (If(Box::new(c), Box::new(th), Box::new(el), (seed, t)), seed+1)
    }
    Let(bindings, body, t) => {
      let (binds, seed) = tag_bindings(bindings, seed);
      let (b, seed) = tag_expr(*body, seed);
      (Let(binds, Box::new(b), (seed, t)), seed+1)
    }
  }
}

fn tag_bindings<T>(bindings: Vec<(usize, Expr<T>)>, mut seed: usize)
                   -> (Vec<(usize, Expr<(usize, T)>)>, usize) {
  let mut ret = Vec::new();
  for (id, expr) in bindings {
    let (ex, s) = tag_expr(expr, seed);
    seed = s;
    ret.push((id, ex));
  }
  (ret, seed)
}

#[derive(Debug, Clone, Copy)]
enum Reg {
  EAX,
  ECX,
  EBP,
  ESP,
}

#[derive(Debug, Clone)]
enum Arg {
  Const(i32),
  HexConst(i32),
  Reg(Reg),
  RegMinus(Reg, usize),
  RegPlus(Reg, usize),
  Sized(ArgSize, Box<Arg>),
}

#[derive(Debug, Clone, Copy)]
enum ArgSize {
  Byte,
  Word,
  Dword,
}

#[derive(Debug)]
enum Instr {
  Mov(Arg, Arg),
  // Inc(Arg),
  // Dec(Arg),
  Add(Arg, Arg),
  Sub(Arg, Arg),
  IMul(Arg, Arg),
  And(Arg, Arg),
  Or(Arg, Arg),
  Xor(Arg, Arg),
  Sar(Arg, Arg),
  Cmp(Arg, Arg),
  Label(String),
  Jmp(String),
  Je(String),
  Jg(String),
  Jge(String),
  Jl(String),
  Jle(String),
  Jo(String),
  Push(Arg),
  Pop(Arg),
  Call(Runtime),
  Ret,
}

#[derive(Debug)]
enum Runtime {
  Print,
  Add1,
  Sub1,
  IsNum,
  IsBool,
  NumCheck,
  NumCheck2,
  BoolCheck,
  BoolCheck2,
  IfCondCheck,
  User(String),
}

/// Return the stack index of symbol ID in ENV.
fn lookup(id: usize, env: &[usize]) -> Option<usize> {
  // Look from the right in order to always get the /latest/ binding
  env.iter().rposition(|&n| n == id).map(|n| n+1)
}

pub fn compile<T>(ast: AST<T>) -> String {
  // First we tag each node with a unique number.  This is needed for compiling
  // IF and for transforming into ANF.  Then we reduce to ANF in order to
  // compile binary expressions.
  let anf_ast = into_anf(number(ast));

  // Then we emit a list of assembly instructions (we renumber since the ANF
  // transformtion lost the numerotation)
  let instrs = compile_ast(&number(anf_ast));

  // And finally we emit ASM as a string
  emit_asm(&instrs)
}

pub fn debug<T>(ast: AST<T>) {
  println!("==== AST ====\n{}", pp(&ast));

  // Same as compile, but printing the intermediate steps.
  let anf_ast = into_anf(number(ast));

  println!("==== ANF ====\n{}", pp(&anf_ast));

  let instrs = compile_ast(&number(anf_ast));

  // And finally we emit ASM as a string
  let asm = emit_asm(&instrs);

  let mut n = 1;
  println!("==== ASM ====\n{}",
           asm.split('\n').map(|line| format!("{:<5}{}", (n, n+=1).0, line))
           .collect::<Vec<String>>().join("\n"));
}

// Return the greatest number of variables we need at once, in order to make
// room on the stack.
fn count_vars<T>(expr: &Expr<T>) -> usize {
  use self::Expr::*;

  match expr {
    // Only `let` can create variables
    Let(bs, e, _)     => bs.len() + count_vars(e),

    Prim1(_, e, _)    => count_vars(e),
    Prim2(_, l, r, _) => usize::max(count_vars(l), count_vars(r)),
    If(cc, th, el, _) => *[count_vars(cc), count_vars(th), count_vars(el)]
      .iter().max().unwrap(),

    _                 => 0
  }
}

fn compile_ast<T>(ast: &AST<(usize, T)>) -> Vec<(String, Vec<Instr>)> {
  let mut v = Vec::new();
  for d in &ast.root.decls {
    v.push((ast.symbols[d.name].clone(), compile_body(&d.body, &ast.symbols,
                                                      &vec![], &d.args)));
  }

  v.push(("entry_point".to_string(), compile_body(&ast.root.body, &ast.symbols,
                                                  &vec![], &vec![])));
  v
}

fn compile_body<T>(body: &Expr<(usize, T)>, symbols: &[String],
                   env: &Vec<usize>, args: &[usize]) -> Vec<Instr> {
  use self::Instr::*;
  use self::Arg::*;
  use self::Reg::*;

  let mut v = vec![
    Push(Reg(EBP)),
    Mov(Reg(EBP), Reg(ESP)),
  ];
  let vars = count_vars(&body);
  if vars > 0 {
      v.push(Sub(Reg(ESP), Const((vars * 4) as i32)));
  }
  v.append(&mut compile_expr(&body, &symbols, env, args));

  v.push(Mov(Reg(ESP), Reg(EBP)));
  v.push(Pop(Reg(EBP)));
  v.push(Ret);
  v
}

static BOOL_TRUE  : i32 = -1;
static BOOL_FALSE : i32 = 0x7fffffff;
static OVERFLOW   : &'static str = "error_overflow";

fn compile_expr<T>(e: &Expr<(usize, T)>, symbols: &[String],
                   env: &Vec<usize>, args: &[usize]) -> Vec<Instr> {
  use self::Instr::*;
  use self::Prim1::*;
  use self::Prim2;
  use self::Arg::*;
  use self::ArgSize::*;
  use self::Reg::*;
  use self::Expr::*;

  match e {
    Number(n, _) => {
      vec![Mov(Reg(EAX), Const(n << 1))]
    }

    Id(s, _) => match lookup(*s, args) {
      // Function arguments are passed by the caller, so are higher in the
      // stack, and the first one start at ebp+ (hence the n+1).
      Some(n) => vec![Mov(Reg(EAX), Sized(Dword, Box::new(RegPlus(EBP, n+1))))],
      None => match lookup(*s, env) {
        // Local variables start and ebp-4 and go down the stack.
        Some(n) => vec![Mov(Reg(EAX), Sized(Dword, Box::new(RegMinus(EBP, n))))],
        None => panic!("Identifier not bound '{}'", symbols[*s]),
      }
    }

    Bool(true, _)  => vec![Mov(Reg(EAX), HexConst(BOOL_TRUE))],
    Bool(false, _) => vec![Mov(Reg(EAX), HexConst(BOOL_FALSE))],

    Prim1(Not, ex, _) => {
      let mut v = compile_expr(ex, symbols, env, args);
      v.push(Push(Reg(EAX)));
      v.push(Call(Runtime::BoolCheck));
      v.push(Pop(Reg(EAX)));
      v.push(Xor(Reg(EAX), HexConst(1 << 31)));
      v
    }

    Prim2(op, l, r, (n, _)) => {
      // If l and r aren't immediate, we cannot compile
      if !is_imm(l) || !is_imm(r) {
        panic!("Binary expression not in ANF");
      }

      let mut v = compile_expr(l, symbols, env, args);
      // Now we know that `r` is immediate, so it's either a Number or an Id,
      // and we can use the right-hand side of the compiled instruction
      // directly to replace the Mov by the adequate arithmetic operation.
      let b = if let Some(Mov(_, b)) = compile_expr(r, symbols, env, args).pop() {
        b
      } else {
        unreachable!();
      };
      // Combine the two, and add runtime checks
      let a = Reg(EAX);

      match op {
        Prim2::Plus | Prim2::Minus | Prim2::Mult |
        Prim2::Greater | Prim2::GreaterEq |
        Prim2::Less | Prim2::LessEq => {
          // So this is tricky: the calling convention wants us to push
          // arguments in reverse order, so intuitively we want to push b,
          // then a.  But!  We need to preserve EAX (a), and we don't care
          // about b.  So, we push a last, so that when popping, the second
          // pop restores EAX.
          v.push(Push(a.clone()));
          v.push(Push(b.clone()));
          v.push(Call(Runtime::NumCheck2));
          v.push(Pop(Reg(EAX)));
          v.push(Pop(Reg(EAX)));
        }

        Prim2::And | Prim2::Or => {
          // Idem
          v.push(Push(a.clone()));
          v.push(Push(b.clone()));
          v.push(Call(Runtime::BoolCheck2));
          v.push(Pop(Reg(EAX)));
          v.push(Pop(Reg(EAX)));
        }

        // Eq compares anything
        Prim2::Eq => {}
      };

      let overflow = OVERFLOW.to_string();

      v.append(&mut match op {
        Prim2::Plus  => vec![Add(a, b), Jo(overflow)],
        Prim2::Minus => vec![Sub(a, b), Jo(overflow)],
        Prim2::Mult  => vec![IMul(a, b), Jo(overflow), Sar(Reg(EAX), Const(1))],
        Prim2::And   => vec![And(a, b)],
        Prim2::Or    => vec![Or(a, b)],
        Prim2::Greater | Prim2::GreaterEq |
        Prim2::Less | Prim2::LessEq | Prim2::Eq => {
          let target = format!("{}_{}", op, n);
          let done = format!("done_{}", n);
          vec![
            Cmp(a, b),
            match op {
              Prim2::Greater   => Jg(target.clone()),
              Prim2::GreaterEq => Jge(target.clone()),
              Prim2::Less      => Jl(target.clone()),
              Prim2::LessEq    => Jle(target.clone()),
              Prim2::Eq        => Je(target.clone()),
              _ => unreachable!(),
            },
            Mov(Reg(EAX), HexConst(BOOL_FALSE)),
            Jmp(done.clone()),
            Label(target),
            Mov(Reg(EAX), HexConst(BOOL_TRUE)),
            Label(done),
          ]
        }
      });
      v
    }

    Apply(name, params, _) => {
      let mut v = Vec::new();

      for a in params {
        let mut v2 = compile_expr(a, symbols, env, args);
        v.append(&mut v2);
        v.push(Push(Reg(EAX)));
      }

      let f = symbols[*name].as_str();
      v.push(Call(match f {
        "add1"   => Runtime::Add1,
        "sub1"   => Runtime::Sub1,
        "print"  => Runtime::Print,
        "isbool" => Runtime::IsBool,
        "isnum"  => Runtime::IsNum,
        _        => Runtime::User(f.to_string()),
      }));

      // Check for overflows for arithmetic functions
      match f {
        "add1" => v.push(Jo(OVERFLOW.to_string())),
        "sub1" => v.push(Jo(OVERFLOW.to_string())),
          _ => {},
      }

      v.push(Add(Reg(ESP), Const(4 * args.len() as i32))); // Discard arguments
      v
    }

    Let(bindings, body, _) => {
      let mut env2 = env.clone();
      let mut v = Vec::new();

      for (x, ex) in bindings {
        v.append(&mut compile_expr(ex, symbols, &env2, args));
        env2.push(*x);
        v.push(Mov(RegMinus(EBP, env2.len()), Reg(EAX)));
      }

      v.append(&mut compile_expr(body, symbols, &mut env2, args));
      v
    }

    If(cond, then, els, (n, _)) => {
      let mut v = Vec::new();
      v.append(&mut compile_expr(cond, symbols, env, args));
      v.push(Push(Reg(EAX)));
      v.push(Call(Runtime::IfCondCheck));
      v.push(Pop(Reg(EAX)));
      v.push(Cmp(Reg(EAX), Const(BOOL_FALSE)));
      let if_false = format!("if_false_{}", n);
      let done = format!("done_{}", n);
      v.push(Je(if_false.clone()));
      v.append(&mut compile_expr(then, symbols, env, args));
      v.push(Jmp(done.clone()));
      v.push(Label(if_false));
      v.append(&mut compile_expr(els, symbols, env, args));
      v.push(Label(done));
      v
    }
  }
}

fn emit_asm(blocks: &[(String, Vec<Instr>)]) -> String {
  format!("section.text
extern is_bool
extern is_num
extern print
extern add1
extern sub1
extern num_check
extern num_check2
extern bool_check
extern bool_check2
extern if_cond_check
extern overflow
global entry_point
{}

{}:
  call overflow",
          blocks.iter().map(|b| format!("{}:\n{}",
                                        b.0,
                                        b.1.iter().map(|i| format!("{}", i)).collect::<String>()))
          .collect::<String>(),
          OVERFLOW)
}

impl Display for Reg {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Reg::*;

    match self {
      EAX => write!(f, "eax"),
      ECX => write!(f, "ecx"),
      EBP => write!(f, "ebp"),
      ESP => write!(f, "esp"),
    }
  }
}

impl Display for Arg {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Arg::*;

    match self {
      Const(n)        => write!(f, "{}", n),
      HexConst(n)     => write!(f, "0x{:x}", n),
      Reg(r)          => write!(f, "{}", r),
      RegMinus(r, o)  => write!(f, "[{}-{}]", r, 4 * o),
      RegPlus(r, o)   => write!(f, "[{}+{}]", r, 4 * o),
      Sized(s, a)     => write!(f, "{} {}", s, a)
    }
  }
}

impl Display for ArgSize {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::ArgSize::*;

    match self {
      Byte  => write!(f, "BYTE"),
      Word  => write!(f, "WORD"),
      Dword => write!(f, "DWORD"),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Instr::*;

    match self {
      Mov(dst, src)  => writeln!(f, "  mov {}, {}", dst, src),
      // Inc(dst)       => writeln!(f, "  inc {}", dst),
      // Dec(dst)       => writeln!(f, "  dec {}", dst),
      Add(dst, src)  => writeln!(f, "  add {}, {}", dst, src),
      Sub(dst, src)  => writeln!(f, "  sub {}, {}", dst, src),
      IMul(dst, src) => writeln!(f, "  imul {}, {}", dst, src),
      And(dst, src)  => writeln!(f, "  and {}, {}", dst, src),
      Or(dst, src)   => writeln!(f, "  or {}, {}", dst, src),
      Xor(dst, src)  => writeln!(f, "  xor {}, {}", dst, src),
      Sar(dst, src)  => writeln!(f, "  sar {}, {}", dst, src),
      Cmp(a, b)      => writeln!(f, "  cmp {}, {}", a, b),
      Label(s)       => writeln!(f, "{}:", s),
      Jmp(s)         => writeln!(f, "  jmp {}", s),
      Je(s)          => writeln!(f, "  je {}", s),
      Jg(s)          => writeln!(f, "  jg {}", s),
      Jge(s)         => writeln!(f, "  jge {}", s),
      Jl(s)          => writeln!(f, "  jl {}", s),
      Jle(s)         => writeln!(f, "  jle {}", s),
      Jo(s)          => writeln!(f, "  jo {}", s),
      Push(s)        => writeln!(f, "  push {}", s),
      Pop(s)         => writeln!(f, "  pop {}", s),
      Call(s)        => writeln!(f, "  call {}", s),
      Ret            => writeln!(f, "  ret"),
    }
  }
}

impl Display for Runtime {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Runtime::*;

    match self {
      Print       => write!(f, "print"),
      Add1        => write!(f, "add1"),
      Sub1        => write!(f, "sub1"),
      IsBool      => write!(f, "is_bool"),
      IsNum       => write!(f, "is_num"),
      NumCheck    => write!(f, "num_check"),
      NumCheck2   => write!(f, "num_check2"),
      BoolCheck   => write!(f, "bool_check"),
      BoolCheck2  => write!(f, "bool_check2"),
      IfCondCheck => write!(f, "if_cond_check"),
      User(n)     => write!(f, "{}", n),
    }
  }
}

impl Display for Prim2 {
   fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Prim2::*;

    match self {
      Greater   => write!(f, "greater"),
      GreaterEq => write!(f, "greater_eq"),
      Less      => write!(f, "less"),
      LessEq    => write!(f, "less_eq"),
      Eq        => write!(f, "eq"),

      _ => unreachable!(),
    }
  }
}

fn new_sym(prefix: &str, tag: usize, symbols: &mut Vec<String>) -> usize {
  let s = format!("{}_{}", prefix, tag);
  symbols.push(s);
  symbols.len() - 1
  }

// Binary expressions of arbitrary size cannot be compiled directly to return to
// EAX.  We transform the program into an Administrative Normal Form (ANF),
// which is composed only of *immediate* expressions which can be trivially
// compiled.

fn is_anf<T>(expr: &Expr<T>) -> bool {
  use self::Expr::*;

  match expr {
    Prim1(_, e, _) => is_anf(e),
    Prim2(_, l, r, _) => is_imm(l) && is_imm(r),
    Let(es, e, _) => es.iter().all(|(_,e)| is_anf(e)) && is_anf(e),
    If(e1, e2, e3, _) => is_imm(e1) && is_anf(e2) && is_anf(e3),
    Apply(_, args, _) => args.iter().all(|a| is_imm(a)),
    e => is_imm(e),
  }
}

fn is_imm<T>(expr: &Expr<T>) -> bool {
  use self::Expr::*;
  match expr {
    Number(_,_) | Id(_,_) | Bool(_,_) => true,
    _ => false
  }
}

// fn replace<T>(a: usize, b: usize, expr: Expr<T>) -> Expr<T> {
//   use self::Expr::*;

//   match expr {
//     Id(n, t) => if n == a { Id(b, t) }
//                 else      { Id(a, t) },
//     Prim1(p, e, t) => Prim1(p, Box::new(replace(a, b, *e)), t),
//     Prim2(p, l, r, t) => Prim2(p,
//                                Box::new(replace(a, b, *l)),
//                                Box::new(replace(a, b, *r)),
//                                t),
//     If(cc, th, el, t) => If(Box::new(replace(a, b, *cc)),
//                             Box::new(replace(a, b, *th)),
//                             Box::new(replace(a, b, *el)),
//                             t),
//     Let(bs, body, t) => {
//       let mut new_bs = Vec::new();
//       let mut shadow = false;
//       for (x,e) in bs {
//         if x == a { shadow = true }
//         if shadow {
//           new_bs.push((x, e));
//         } else {
//           new_bs.push((x, replace(a, b, e)));
//         }
//       }
//       Let(new_bs, Box::new(if shadow { *body }
//                            else      { replace(a, b, *body) }), t)
//     }
//     _ => expr,
//   }
// }

// This helper will decompose an expression of arbitrary depth into ANF, where
// all immediate expressions are put in a single context (second returned arg).
// After that, into_anf will simply create a single Let expr with this context
// as bindings.
fn into_anf1<T>(expr: Expr<(usize, T)>, symbols: &mut Vec<String>)
                -> (Expr<()>, Vec<(usize, Expr<()>)>) {
  use self::Expr::*;

  match expr {
    Number(n, _) => (Number(n, ()), vec![]),
    Id(s, _) => (Id(s, ()), vec![]),
    Bool(b, _) => (Bool(b, ()), vec![]),
    Prim1(p, e, _) => {
      let (imm, ctx) = into_anf1(*e, symbols);
      (Prim1(p, Box::new(imm), ()), ctx)
    },

    Prim2(p, l, r, (t, _)) => {
      let (l_imm, mut l_ctx) = into_anf1(*l, symbols);
      let (r_imm, mut r_ctx) = into_anf1(*r, symbols);
      let s = new_sym("prim2", t, symbols);
      l_ctx.append(&mut r_ctx);
      l_ctx.push((s, Prim2(p, Box::new(l_imm), Box::new(r_imm), ())));
      (Id(s, ()), l_ctx)
    }

    Apply(name, args, (t, _)) => {
      let mut ctx = Vec::new();
      let mut args_syms = Vec::new();
      for a in args {
        let (a_anf, mut c) = into_anf1(a, symbols);
        ctx.append(&mut c);
        args_syms.push(a_anf);
      }

      let s = new_sym("call", t, symbols);
      ctx.push((s, Apply(name, args_syms, ())));

      (Id(s, ()), ctx)
    }

    // For Let, we can 'flatten' the context gotten from the recursion.  Instead
    // of:
    //
    //   let a = 1 + 2 + 3
    //   =>
    //   let c1 = 1 + 2, c2 = c1 + 3 in let a = c2 in a
    //
    // We transform directly to:
    //
    //  let c1 = 1 + 2, c2 = c1 + 3, a = c2 in a
    //
    // This transformation is incorrect if shadowing is allowed.
    Let(bs, e, (t, _)) => {
      let mut ctx = vec![];
      for (x,b) in bs {
        let (imm, mut c) = into_anf1(b, symbols);
        ctx.append(&mut c);
        ctx.push((x, imm));
      }
      let (e_anf, mut c) = into_anf1(*e, symbols);
      ctx.append(&mut c);
      let s = new_sym("let", t, symbols);
      ctx.push((s, e_anf));
      (Id(s, ()), ctx)
    }

    If(cc, th, el, (t, _)) => {
      let (cc_anf, mut ctx) = into_anf1(*cc, symbols);
      let s = new_sym("if", t, symbols);
      ctx.push((s, cc_anf));
      let (th_anf, th_ctx) = into_anf1(*th, symbols);
      let (el_anf, el_ctx) = into_anf1(*el, symbols);
      (If(Box::new(Id(s, ())),
          Box::new(if th_ctx.len() > 0 { Let(th_ctx, Box::new(th_anf), ()) }
                   else                { th_anf }),
          Box::new(if el_ctx.len() > 0 { Let(el_ctx, Box::new(el_anf), ()) }
                   else                { el_anf }),
          ()),
       ctx)
    }
  }
}

fn into_anf_expr<T>(expr: Expr<(usize, T)>, symbols: &mut Vec<String>) -> Expr<()> {
  let (e, ctx) = into_anf1(expr, symbols);

  let root = if ctx.len() == 0 {
    e
  } else {
    self::Expr::Let(ctx, Box::new(e), ())
  };

  if !is_anf(&root) {
    panic!("Normalization into ANF failed");
  } else {
    root
  }
}

fn into_anf<T>(mut ast: AST<(usize, T)>) -> AST<()> {
  // First convert all declarations into anf

  let mut decls = Vec::new();

  for d in ast.root.decls {
    let body = into_anf_expr(d.body, &mut ast.symbols);
    decls.push(Decl {
      name: d.name,
      args: d.args,
      body
    })
  }

  let body = into_anf_expr(ast.root.body, &mut ast.symbols);

  AST {
    root: Prog { decls, body },
    symbols: ast.symbols,
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Pretty printer
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fn pp<T>(ast: &AST<T>) -> String {
  format!("{}\n\n{}",
          ast.root.decls.iter()
          .map(|d| pp_decl(d, &ast.symbols))
          .collect::<Vec<String>>().join("\n"),
          pp_expr(&ast.root.body, &ast.symbols))
}

fn pp_decl<T>(decl: &Decl<T>, symbols: &[String]) -> String {
  format!("def {}({}):\n{}",
          symbols[decl.name],
          decl.args.iter()
          .map(|x| format!("{}", symbols[*x]))
          .collect::<Vec<String>>().join(","),
          pp_expr(&decl.body, symbols))
}

fn pp_expr<T>(expr: &Expr<T>, symbols: &[String]) -> String {
  use self::Expr::*;
  use self::Prim2::*;

  match expr {
    Number(n, _) => format!("{}", n),
    Id(n, _) => format!("{}", symbols[*n]),
    Bool(b, _) => format!("{}", b),
    Prim1(p, e, _) => format!("{:?}({})", p, pp_expr(e, symbols)),
    Prim2(p, l, r, _) => format!("{} {} {}",
                                 pp_expr(l, symbols),
                                 match p {
                                   Plus      => "+",
                                   Minus     => "-",
                                   Mult      => "*",
                                   And       => "&&",
                                   Or        => "||",
                                   Less      => "<",
                                   LessEq    => "<=",
                                   Greater   => ">",
                                   GreaterEq => ">=",
                                   Eq        => "==",
                                 },
                                 pp_expr(r, symbols)),
    Apply(s, es, _) => format!("{}({})", symbols[*s],
                               es.iter().map(|e| pp_expr(e, symbols))
                               .collect::<Vec<String>>().join(", ")),
    Let(bs, body, _) => format!("let {} in {}",
                                bs.iter()
                                .map(|(x,e)| format!("{} = {}",
                                                     symbols[*x],
                                                     pp_expr(e, symbols)))
                                .collect::<Vec<String>>().join(",\n    "),
                                pp_expr(body, symbols)),
    If(cc, th, el, _) => format!("if {}:\n{}\nelse:\n{}",
                                 pp_expr(cc, symbols),
                                 pp_expr(th, symbols),
                                 pp_expr(el, symbols)),
  }
}
