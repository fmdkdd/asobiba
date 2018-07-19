use std::fmt::Display;

use parse::{AST, Expr, Prim1, Prim2};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compiler
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Number each node of the AST
fn number<T>(ast: AST<T>) -> AST<(usize, T)> {
  AST {
    root: tag_expr(ast.root, 1).0,
    symbols: ast.symbols.clone(),
  }
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

fn tag<T>(expr: &Expr<T>) -> &T {
  use self::Expr::*;

  match expr {
    Number(_, t)      => t,
    Id(_, t)          => t,
    Bool(_, t)        => t,
    Prim1(_, _, t)    => t,
    Prim2(_, _, _, t) => t,
    Let(_, _, t)      => t,
    If(_, _, _, t)    => t,
  }
}

#[derive(Debug)]
enum Reg {
  EAX,
  ESP,
}

#[derive(Debug)]
enum Arg {
  Const(i32),
  HexConst(i32),
  Reg(Reg),
  RegOffset(Reg, usize),
}

#[derive(Debug)]
enum Instr {
  Mov(Arg, Arg),
  Inc(Arg),
  Dec(Arg),
  Add(Arg, Arg),
  Sub(Arg, Arg),
  IMul(Arg, Arg),
  And(Arg, Arg),
  Or(Arg, Arg),
  Xor(Arg, Arg),
  Shr(Arg, Arg),
  Cmp(Arg, Arg),
  Label(String),
  Jmp(String),
  Je(String),
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

  println!("==== ASM ====\n{}", asm);
}

fn compile_ast<T>(ast: &AST<(usize, T)>) -> Vec<Instr> {
  compile_expr(&ast.root, &ast.symbols, &mut vec![])
}

fn compile_expr<T>(e: &Expr<(usize, T)>, symbols: &[String], env: &Vec<usize>) -> Vec<Instr> {
  use self::Instr::*;
  use self::Prim1::*;
  use self::Prim2;
  use self::Arg::*;
  use self::Reg::*;
  use self::Expr::*;

  match e {
    Number(n, _) => {
      if *n > ((1 << 30) - 1) || *n < -(1 << 30) {
        panic!("Integer is too large to be represented: {}", n);
      } else {
        vec![Mov(Reg(EAX), Const(n << 1))]
      }
    }

    Id(s, _) => match lookup(*s, env) {
      Some(n) => vec![Mov(Reg(EAX), RegOffset(ESP, n))],
      None => panic!("Identifier not bound '{}'", symbols[*s]),
    }

    Bool(true, _)  => vec![Mov(Reg(EAX), HexConst(-1))],
    Bool(false, _) => vec![Mov(Reg(EAX), HexConst(0x7FFFFFFF))],

    Prim1(Add1, ex, _) => {
      let mut v = compile_expr(ex, symbols,env);
      v.push(Add(Reg(EAX), Const(2)));
      v
    }

    Prim1(Sub1, ex, _) => {
      let mut v = compile_expr(ex, symbols, env);
      v.push(Sub(Reg(EAX), Const(2)));
      v
    }

    Prim1(Not, ex, _) => {
      let mut v = compile_expr(ex, symbols, env);
      v.push(Xor(Reg(EAX), HexConst(1 << 31)));
      v
    }

    Prim1(_, _, _) => unimplemented!(),

    Prim2(op, l, r, _) => {
      // If l and r aren't immediate, we cannot compile
      if !is_imm(l) || !is_imm(r) {
        panic!("Binary expression not in ANF");
      }

      let mut v = compile_expr(l, symbols, env);
      // Now we know that `r` is immediate, so it's either a Number or an Id,
      // and we can use the right-hand side of the compiled instruction
      // directly to replace the Mov by the adequate arithmetic operation.
      let b = if let Some(Mov(_, b)) = compile_expr(r, symbols, env).pop() {
        b
      } else {
        unreachable!();
      };
      // Combine the two
      let a = Reg(EAX);
      v.append(&mut match op {
        Prim2::Plus  => vec![Add(a, b)],
        Prim2::Minus => vec![Sub(a, b)],
        Prim2::Mult  => vec![IMul(a, b), Shr(Reg(EAX), Const(1))],
        Prim2::And   => vec![And(a, b)],
        Prim2::Or    => vec![Or(a, b)],
        _ => unimplemented!(),
      });
      v
    }

    Let(bindings, body, _) => {
      let mut env2 = env.clone();
      let mut v = Vec::new();

      // Check for duplicate bindings first, which are forbidden by the
      // language.
      let mut b : Vec<usize> = bindings.iter().map(|&(id,_)| id).collect();
      b.sort();
      b.dedup();
      if b.len() != bindings.len() {
        panic!("Duplicate bindings in `let`");
      }

      for (x, ex) in bindings {
        v.append(&mut compile_expr(ex, symbols, &env2));
        env2.push(*x);
        v.push(Mov(RegOffset(ESP, env2.len()), Reg(EAX)));
      }

      v.append(&mut compile_expr(body, symbols, &mut env2));
      v
    }

    If(cond, then, els, (n, _)) => {
      let mut v = Vec::new();
      v.append(&mut compile_expr(cond, symbols, env));
      v.push(Cmp(Reg(EAX), Const(0)));
      let if_false = format!("if_false_{}", n);
      let done = format!("done_{}", n);
      v.push(Je(if_false.clone()));
      v.append(&mut compile_expr(then, symbols, env));
      v.push(Jmp(done.clone()));
      v.push(Label(if_false));
      v.append(&mut compile_expr(els, symbols, env));
      v.push(Label(done));
      v
    }
  }
}

fn emit_asm(instrs: &[Instr]) -> String {
  format!("section.text
global entry_point
entry_point:
{}
  ret", instrs.iter().map(|i| format!("{}", i)).collect::<String>())
}

impl Display for Reg {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Reg::*;

    match self {
      EAX => write!(f, "eax"),
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
      RegOffset(r, o) => write!(f, "[{} - 4*{}]", r, o),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Instr::*;

    match self {
      Mov(dst, src)  => writeln!(f, "  mov {}, {}", dst, src),
      Inc(dst)       => writeln!(f, "  inc {}", dst),
      Dec(dst)       => writeln!(f, "  dec {}", dst),
      Add(dst, src)  => writeln!(f, "  add {}, {}", dst, src),
      Sub(dst, src)  => writeln!(f, "  sub {}, {}", dst, src),
      IMul(dst, src) => writeln!(f, "  imul {}, {}", dst, src),
      And(dst, src)  => writeln!(f, "  and {}, {}", dst, src),
      Or(dst, src)   => writeln!(f, "  or {}, {}", dst, src),
      Xor(dst, src)  => writeln!(f, "  xor {}, {}", dst, src),
      Shr(dst, src)  => writeln!(f, "  shr {}, {}", dst, src),
      Cmp(a, b)      => writeln!(f, "  cmp {}, {}", a, b),
      Label(s)       => writeln!(f, "{}:", s),
      Jmp(s)         => writeln!(f, "  jmp {}", s),
      Je(s)          => writeln!(f, "  je {}", s),
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

fn replace<T>(a: usize, b: usize, expr: Expr<T>) -> Expr<T> {
  use self::Expr::*;

  match expr {
    Id(n, t) => if n == a { Id(b, t) }
                else      { Id(a, t) },
    Prim1(p, e, t) => Prim1(p, Box::new(replace(a, b, *e)), t),
    Prim2(p, l, r, t) => Prim2(p,
                               Box::new(replace(a, b, *l)),
                               Box::new(replace(a, b, *r)),
                               t),
    If(cc, th, el, t) => If(Box::new(replace(a, b, *cc)),
                            Box::new(replace(a, b, *th)),
                            Box::new(replace(a, b, *el)),
                            t),
    Let(bs, body, t) => {
      let mut new_bs = Vec::new();
      let mut shadow = false;
      for (x,e) in bs {
        if x == a { shadow = true }
        if shadow {
          new_bs.push((x, e));
        } else {
          new_bs.push((x, replace(a, b, e)));
        }
      }
      Let(new_bs, Box::new(if shadow { *body }
                           else      { replace(a, b, *body) }), t)
    }
    _ => expr,
  }
}

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
      let (imm, mut ctx) = into_anf1(*e, symbols);
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

fn into_anf<T>(mut ast: AST<(usize, T)>) -> AST<()> {
  let (e, ctx) = into_anf1(ast.root, &mut ast.symbols);

  let root = if ctx.len() == 0 {
    e
  } else {
    self::Expr::Let(ctx, Box::new(e), ())
  };

  let ast_anf = AST {
    root: root,
    symbols: ast.symbols,
  };

  if !is_anf(&ast_anf.root) {
    panic!("Normalization into ANF failed");
  } else {
    ast_anf
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Pretty printer
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fn pp<T>(ast: &AST<T>) -> String {
  pp_expr(&ast.root, &ast.symbols)
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
                                 pp_expr(el, symbols))
  }
}
