use std::fmt::Display;

use crate::parse::{AST, Decl, Expr, Prim1, Prim2, Prog};


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tagging
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
    loc: decl.loc,
    body,
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


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tagging for ANF
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Number each node of the AST
fn number_anf<T>(ast: ANF_AST<T>) -> ANF_AST<(usize, T)> {
  ANF_AST {
    root: tag_anf_prog(ast.root, 1).0,
    symbols: ast.symbols.clone(),
  }
}

fn tag_anf_prog<T>(prog: ANF_Prog<T>, seed: usize) -> (ANF_Prog<(usize, T)>, usize) {
  let mut decls = Vec::new();

  let mut s = seed;
  for d in prog.decls {
    let (d_t, s1) = tag_anf_decl(d, s);
    s = s1;
    decls.push(d_t);
  }

  let (body, s1) = tag_aexpr(prog.body, s);
  s = s1;

  (ANF_Prog { decls, body }, s)
}

fn tag_anf_decl<T>(decl: ANF_Decl<T>, seed: usize) -> (ANF_Decl<(usize, T)>, usize) {

  let (body, seed) = tag_aexpr(decl.body, seed);

  (ANF_Decl {
    name: decl.name,
    args: decl.args,
    body,
  }, seed)
}

fn tag_aexpr<T>(expr: AExpr<T>, seed: usize) -> (AExpr<(usize, T)>, usize) {
  use self::AExpr::*;

  match expr {
    Let(b, e, body, t) => {
      let (e, seed) = tag_cexpr(e, seed);
      let (body, seed) = tag_aexpr(*body, seed);
      (Let(b, e, Box::new(body), (seed, t)), seed+1)
    }

    Expr(e) => {
      let (e, seed) = tag_cexpr(e, seed);
      (Expr(e), seed)
    }
  }
}

fn tag_cexpr<T>(expr: CExpr<T>, seed: usize) -> (CExpr<(usize, T)>, usize) {
  use self::CExpr::*;

  match expr {
    Imm(e) => {
      let (e, seed) = tag_immexpr(e, seed);
      (Imm(e), seed)
    }

    Prim1(op, exp, t) => {
      let (e, seed) = tag_immexpr(exp, seed);
      (Prim1(op, e, (seed, t)), seed+1)
    }
    Prim2(op, left, right, t) => {
      let (l, seed) = tag_immexpr(left, seed);
      let (r, seed) = tag_immexpr(right, seed);
      (Prim2(op, l, r, (seed, t)), seed+1)
    }
    Apply(id, exps, t) => {
      let mut exps_t = Vec::new();
      let mut seed = seed;
      for e in exps {
        let (e_t, s1) = tag_immexpr(e, seed);
        exps_t.push(e_t);
        seed = s1;
      }
      (Apply(id, exps_t, (seed, t)), seed+1)
    }
    If(cond, then, els, t) => {
      let (c, seed) = tag_immexpr(cond, seed);
      let (th, seed) = tag_aexpr(*then, seed);
      let (el, seed) = tag_aexpr(*els, seed);
      (If(c, Box::new(th), Box::new(el), (seed, t)), seed+1)
    }
  }
}

fn tag_immexpr<T>(expr: ImmExpr<T>, seed: usize) -> (ImmExpr<(usize, T)>, usize) {
  use self::ImmExpr::*;

  match expr {
    Number(n, t) => (Number(n, (seed, t)), seed+1),
    Id(s, t) => (Id(s, (seed, t)), seed+1),
    Bool(b, t) => (Bool(b, (seed, t)), seed+1),
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ASM representation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#[derive(Debug, Clone)]
enum Runtime {
  Print,
  Add1,
  Sub1,
  IsNum,
  IsBool,
  NumCheck,
  BoolCheck,
  IfCondCheck,
  User(String),
}

/// Return the stack index of symbol ID in ENV.
fn lookup(id: usize, env: &[usize]) -> Option<usize> {
  // Look from the right in order to always get the /latest/ binding
  env.iter().rposition(|&n| n == id).map(|n| n+1)
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Compilation: turning the AST into an ASM string
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pub fn compile<T>(ast: AST<T>) -> String {
  // First we tag each node with a unique number.  This is needed for compiling
  // IF and for transforming into ANF.  Then we reduce to ANF in order to
  // compile binary expressions.
  let anf_ast = into_anf(number(ast));

  // Then we emit a list of assembly instructions (we renumber since the ANF
  // transformtion lost the numerotation)
  let instrs = compile_ast(&number_anf(anf_ast));

  // And finally we emit ASM as a string
  emit_asm(&instrs)
}

pub fn debug<T>(ast: AST<T>) {
  println!("==== AST ====\n{}", pp(&ast));

  // Same as compile, but printing the intermediate steps.
  let anf_ast = into_anf(number(ast));

  println!("==== ANF ====\n{}", pp_anf(&anf_ast));

  let instrs = compile_ast(&number_anf(anf_ast));

  // And finally we emit ASM as a string
  let asm = emit_asm(&instrs);

  let mut n = 1;
  println!("==== ASM ====\n{}",
           asm.split('\n').map(|line| format!("{:<5}{}", (n, n+=1).0, line))
           .collect::<Vec<String>>().join("\n"));
}

// Return the greatest number of variables we need at once, in order to make
// room on the stack.
fn count_vars<T>(expr: &AExpr<T>) -> usize {
  use self::AExpr::*;
  use self::CExpr::*;

  match expr {
    // Only `let` can create variables
    Let(_, _, body, _)       => 1 + count_vars(body),
    // Paths in `if` are exclusive, so we only have to account to the larger
    // number of variables used in them
    Expr(If(_, thn, els, _)) => usize::max(count_vars(thn), count_vars(els)),
    // Other expressions cannot contain `let`
    _                        => 0
  }
}

fn compile_ast<T>(ast: &ANF_AST<(usize, T)>) -> Vec<(String, Vec<Instr>)> {
  let mut v = Vec::new();
  for d in &ast.root.decls {
    v.push((ast.symbols[d.name].clone(), compile_body(&d.body, &ast.symbols,
                                                      &vec![], &d.args)));
  }

  v.push(("entry_point".to_string(), compile_body(&ast.root.body, &ast.symbols,
                                                  &vec![], &vec![])));
  v
}

fn compile_body<T>(body: &AExpr<(usize, T)>, symbols: &[String],
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
  v.append(&mut compile_aexpr(&body, &symbols, env, args));

  v.push(Mov(Reg(ESP), Reg(EBP)));
  v.push(Pop(Reg(EBP)));
  v.push(Ret);
  v
}

static BOOL_TRUE  : i32 = -1;
static BOOL_FALSE : i32 = 0x7fffffff;
static OVERFLOW   : &'static str = "error_overflow";

fn compile_aexpr<T>(e: &AExpr<(usize, T)>, symbols: &[String],
                    env: &Vec<usize>, args: &[usize]) -> Vec<Instr> {
  use self::Instr::*;
  use self::Arg::*;
  use self::Reg::*;
  use self::AExpr::*;

  match e {
    Let(b, e, body, _) => {
      let mut env2 = env.clone();
      let mut v = Vec::new();
      v.append(&mut compile_cexpr(e, symbols, &env2, args));
      env2.push(*b);
      v.push(Mov(RegMinus(EBP, env2.len()), Reg(EAX)));
      v.append(&mut compile_aexpr(body, symbols, &mut env2, args));
      v
    }

    Expr(e) => compile_cexpr(e, symbols, env, args)
  }
}

fn compile_cexpr<T>(e: &CExpr<(usize, T)>, symbols: &[String],
                    env: &Vec<usize>, args: &[usize]) -> Vec<Instr> {
  use self::Instr::*;
  use self::Arg::*;
  use self::Reg::*;
  use self::Prim1;
  use self::Prim2;
  use self::CExpr::*;

  match e {
    Imm(e) => {
      let a = compile_immexpr(e, symbols, env, args);
      vec![Mov(Reg(EAX), a)]
    }

    Prim1(Prim1::Not, ex, _) => {
      let a = compile_immexpr(ex, symbols, env, args);
      vec![
        Push(a),
        Call(Runtime::BoolCheck),
        Pop(Reg(EAX)),
        Xor(Reg(EAX), HexConst(1 << 31)),
      ]
    }

    Prim2(op, l, r, (n, _)) => {
      // Figure out the runtime check we need to call
      let check = match op {
        Prim2::Eq => None, // Eq doesn't need runtime checks
        _ =>
          match op {
            Prim2::And | Prim2::Or => Some(Runtime::BoolCheck),
            _                      => Some(Runtime::NumCheck),
          }
      };

      // Compile l, add runtime checks
      let l = compile_immexpr(l, symbols, env, args);

      let mut v = Vec::new();
      // Save a to the stack, as `b` will overwrite EAX
      v.push(Push(l));
      if let Some(ref c) = check {
        v.push(Call(c.clone()));
      }

      // Compile r, add runtime checks, then put it in EAX
      let r = compile_immexpr(r, symbols, env, args);
      v.push(Push(r));
      if let Some(c) = check {
        v.push(Call(c));
      }
      v.push(Pop(Reg(ECX))); // b in ECX
      v.push(Pop(Reg(EAX))); // a in EAX

      let a = Reg(EAX);
      let b = Reg(ECX);
      let overflow = OVERFLOW.to_string();
      v.append(&mut match op {
        Prim2::Plus  => vec![Add(a, b), Jo(overflow)],
        Prim2::Minus => vec![Sub(a, b), Jo(overflow)],
        Prim2::Mult  => vec![IMul(a, b), Jo(overflow), Sar(Reg(EAX), Const(1))],
        Prim2::And   => vec![And(a, b)],
        Prim2::Or    => vec![Or(a, b)],
        Prim2::Greater | Prim2::GreaterEq |
        Prim2::Less | Prim2::LessEq | Prim2::Eq => {
          let target = format!("{:?}_{}", op, n);
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

      for a in params.iter().rev() {
        let arg = compile_immexpr(a, symbols, env, args);
        v.push(Push(arg));
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

      v.push(Add(Reg(ESP), Const(4 * params.len() as i32))); // Discard arguments
      v
    }

    If(cond, then, els, (n, _)) => {
      let mut v = Vec::new();
      let c = compile_immexpr(cond, symbols, env, args);
      v.push(Push(c));
      v.push(Call(Runtime::IfCondCheck));
      v.push(Pop(Reg(EAX)));
      v.push(Cmp(Reg(EAX), HexConst(BOOL_FALSE)));
      let if_false = format!("if_false_{}", n);
      let done = format!("done_{}", n);
      v.push(Je(if_false.clone()));
      v.append(&mut compile_aexpr(then, symbols, env, args));
      v.push(Jmp(done.clone()));
      v.push(Label(if_false));
      v.append(&mut compile_aexpr(els, symbols, env, args));
      v.push(Label(done));
      v
    }
  }
}

fn compile_immexpr<T>(e: &ImmExpr<(usize, T)>, symbols: &[String],
                      env: &Vec<usize>, args: &[usize]) -> Arg {
  use self::Arg::*;
  use self::ArgSize::*;
  use self::Reg::*;
  use self::ImmExpr::*;

  match e {
    Number(n, _) => Const(n << 1),

    Id(s, _) => match lookup(*s, args) {
      // Function arguments are passed by the caller, so are higher in the
      // stack, and the first one start at ebp+ (hence the n+1).
      Some(n) => Sized(Dword, Box::new(RegPlus(EBP, n+1))),
      None => match lookup(*s, env) {
        // Local variables start and ebp-4 and go down the stack.
        Some(n) => Sized(Dword, Box::new(RegMinus(EBP, n))),
        None => panic!("Identifier not bound '{}'", symbols[*s]),
      }
    }

    Bool(true, _)  => HexConst(BOOL_TRUE),
    Bool(false, _) => HexConst(BOOL_FALSE),
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
extern bool_check
extern if_cond_check
extern overflow
global entry_point

{}
{}:
  call overflow",
          blocks.iter().map(|b| format!("{}:\n{}\n",
                                        b.0,
                                        b.1.iter().map(|i| format!("{}", i)).collect::<String>()))
          .collect::<String>(),
          OVERFLOW)
}

fn new_sym(prefix: &str, tag: usize, symbols: &mut Vec<String>) -> usize {
  let s = format!("{}_{}", prefix, tag);
  symbols.push(s);
  symbols.len() - 1
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Transformation into ANF
//
// Binary expressions of arbitrary size cannot be compiled directly to return to
// EAX.  We transform the program into an Administrative Normal Form (ANF),
// which is composed only of *immediate* expressions which can be trivially
// compiled.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Immediate expressions
#[derive(Debug, PartialEq)]
pub enum ImmExpr<T> {
  Number(i32, T),
  Id(usize, T),
  Bool(bool, T),
}

// Compound expressions
#[derive(Debug, PartialEq)]
pub enum CExpr<T> {
  Imm(ImmExpr<T>),
  Prim1(Prim1, ImmExpr<T>, T),
  Prim2(Prim2, ImmExpr<T>, ImmExpr<T>, T),
  Apply(usize, Vec<ImmExpr<T>>, T),
  If(ImmExpr<T>, Box<AExpr<T>>, Box<AExpr<T>>, T),
}

// A-Normal form expressions
#[derive(Debug, PartialEq)]
pub enum AExpr<T> {
  Expr(CExpr<T>),
  Let(usize, CExpr<T>, Box<AExpr<T>>, T),
}

#[derive(Debug, PartialEq)]
pub struct ANF_Decl<T> {
  pub name: usize,
  pub args: Vec<usize>,
  pub body: AExpr<T>,
}

#[derive(Debug, PartialEq)]
pub struct ANF_Prog<T> {
  pub decls: Vec<ANF_Decl<T>>,
  pub body: AExpr<T>,
}

struct ANF_AST<T> {
  root: ANF_Prog<T>,
  symbols: Vec<String>,
}

fn expr_to_immexpr<T>(expr: &Expr<(usize, T)>, symbols: &mut Vec<String>)
                      -> (ImmExpr<()>, Vec<(usize, CExpr<()>)>) {
  match expr {
    Expr::Number(n, _) => (ImmExpr::Number(*n, ()), vec![]),
    Expr::Id(s, _)     => (ImmExpr::Id(*s, ()),     vec![]),
    Expr::Bool(b, _)   => (ImmExpr::Bool(*b, ()),   vec![]),

    Expr::Prim1(op, e, (t, _)) => {
      let (imm, mut setup) = expr_to_immexpr(e, symbols);
      let s = new_sym(format!("{:?}", op).as_str(), *t, symbols);
      setup.push((s, CExpr::Prim1(*op, imm, ())));
      (ImmExpr::Id(s, ()), setup)
    }

    Expr::Prim2(op, l, r, (t, _)) => {
      let (l_imm, mut setup) = expr_to_immexpr(l, symbols);
      let (r_imm, mut setup2) = expr_to_immexpr(r, symbols);
      setup.append(&mut setup2);
      let s = new_sym(format!("{:?}", op).as_str(), *t, symbols);
      setup.push((s, CExpr::Prim2(*op, l_imm, r_imm, ())));
      (ImmExpr::Id(s, ()), setup)
    }

    Expr::Apply(f, args, (t, _)) => {
      let mut args_imm = vec![];
      let mut setup = vec![];
      for a in args {
        let (imm, mut setup2) = expr_to_immexpr(a, symbols);
        setup.append(&mut setup2);
        args_imm.push(imm);
      }
      let s = new_sym("apply", *t, symbols);
      setup.push((s, CExpr::Apply(*f, args_imm, ())));
      (ImmExpr::Id(s, ()), setup)
    }

    Expr::If(cnd, thn, els, (t, _)) => {
      let (cnd_imm, mut setup) = expr_to_immexpr(cnd, symbols);
      let thn_blk = expr_to_aexpr(thn, symbols);
      let els_blk = expr_to_aexpr(els, symbols);

      let s = new_sym("if", *t, symbols);
      setup.push((s, CExpr::If(cnd_imm, Box::new(thn_blk), Box::new(els_blk), ())));
      (ImmExpr::Id(s, ()), setup)
    }

    Expr::Let(bindings, body, _) => {
      let mut setup = vec![];
      for (x, e) in bindings {
        let (e_c, mut setup2) = expr_to_cexpr(e, symbols);
        setup.append(&mut setup2);
        setup.push((*x, e_c));
      }
      let (body_imm, mut setup2) = expr_to_immexpr(body, symbols);
      setup.append(&mut setup2);
      (body_imm, setup)
    }
  }
}

fn expr_to_cexpr<T>(expr: &Expr<(usize, T)>, symbols: &mut Vec<String>)
                    -> (CExpr<()>, Vec<(usize, CExpr<()>)>) {
  match expr {
    Expr::Number(_, _) | Expr::Id(_, _) | Expr::Bool(_, _) => {
      let (imm, setup) = expr_to_immexpr(expr, symbols);
      (CExpr::Imm(imm), setup)
    },

    Expr::Prim1(op, e, _) => {
      let (imm, setup) = expr_to_immexpr(e, symbols);
      (CExpr::Prim1(*op, imm, ()), setup)
    }

    Expr::Prim2(op, l, r, _) => {
      let (l_imm, mut setup) = expr_to_immexpr(l, symbols);
      let (r_imm, mut setup2) = expr_to_immexpr(r, symbols);
      setup.append(&mut setup2);
      (CExpr::Prim2(*op, l_imm, r_imm, ()), setup)
    }

    Expr::Apply(f, args, _) => {
      let mut args_imm = vec![];
      let mut setup = vec![];
      for a in args {
        let (imm, mut setup2) = expr_to_immexpr(a, symbols);
        setup.append(&mut setup2);
        args_imm.push(imm);
      }
      (CExpr::Apply(*f, args_imm, ()), setup)
    }

    Expr::If(cnd, thn, els, _) => {
      let (cnd_imm, setup) = expr_to_immexpr(cnd, symbols);
      let thn_blk = expr_to_aexpr(thn, symbols);
      let els_blk = expr_to_aexpr(els, symbols);
      (CExpr::If(cnd_imm, Box::new(thn_blk), Box::new(els_blk), ()), setup)
    }

    Expr::Let(bindings, body, _) => {
      let mut setup = vec![];
      for (x, e) in bindings {
        let (e_c, mut setup2) = expr_to_cexpr(e, symbols);
        setup.append(&mut setup2);
        setup.push((*x, e_c));
      }
      let (body_imm, mut setup2) = expr_to_cexpr(body, symbols);
      setup.append(&mut setup2);
      (body_imm, setup)
    }
  }
}

fn expr_to_aexpr<T>(expr: &Expr<(usize, T)>, symbols: &mut Vec<String>) -> AExpr<()> {
  let (body, setup) = expr_to_cexpr(expr, symbols);

  let mut ret = AExpr::Expr(body);
  for (x, b) in setup.into_iter().rev() {
    ret = AExpr::Let(x, b, Box::new(ret), ());
  }

  ret
}

fn into_anf<T>(mut ast: AST<(usize, T)>) -> ANF_AST<()> {
  // First convert all declarations into anf
  let mut decls = Vec::new();
  for d in ast.root.decls {
    let body = expr_to_aexpr(&d.body, &mut ast.symbols);
    decls.push(ANF_Decl {
      name: d.name,
      args: d.args,
      body,
    })
  }

  // Then the body of the program
  let body = expr_to_aexpr(&ast.root.body, &mut ast.symbols);

  ANF_AST {
    root: ANF_Prog { decls, body },
    symbols: ast.symbols,
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Pretty printer
//
// We can't simply implement the Display trait for AST, because we need to pass
// down the symbols vec to Expr in order to pretty-print identifiers.
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

  match expr {
    Number(n, _) => format!("{}", n),
    Id(n, _) => format!("{}", symbols[*n]),
    Bool(b, _) => format!("{}", b),
    Prim1(op, e, _) => format!("{}{}", op, pp_expr(e, symbols)),
    Prim2(op, l, r, _) => format!("{} {} {}",
                                  pp_expr(l, symbols),
                                  op,
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


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ANF Pretty printer
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fn pp_anf<T>(ast: &ANF_AST<T>) -> String {
  format!("{}\n\n{}",
          ast.root.decls.iter()
          .map(|d| pp_anf_decl(d, &ast.symbols))
          .collect::<Vec<String>>().join("\n"),
          pp_aexpr(&ast.root.body, &ast.symbols))
}

fn pp_anf_decl<T>(decl: &ANF_Decl<T>, symbols: &[String]) -> String {
  format!("def {}({}):\n{}",
          symbols[decl.name],
          decl.args.iter()
          .map(|x| format!("{}", symbols[*x]))
          .collect::<Vec<String>>().join(","),
          pp_aexpr(&decl.body, symbols))
}

fn pp_aexpr<T>(expr: &AExpr<T>, symbols: &[String]) -> String {
  use self::AExpr::*;

  match expr {
    Expr(e) => pp_cexpr(e, symbols),
    Let(x, e, body, _) => format!("let {} = {} in\n{}",
                                  symbols[*x],
                                  pp_cexpr(e, symbols),
                                  pp_aexpr(body, symbols)),
  }
}

fn pp_cexpr<T>(expr: &CExpr<T>, symbols: &[String]) -> String {
  use self::CExpr::*;

  match expr {
    Imm(e) => format!("{}", pp_immexpr(e, symbols)),
    Prim1(op, e, _) => format!("{}{}", op, pp_immexpr(e, symbols)),
    Prim2(op, l, r, _) => format!("{} {} {}",
                                  pp_immexpr(l, symbols),
                                  op,
                                  pp_immexpr(r, symbols)),
    Apply(f, args, _) => format!("{}({})", symbols[*f],
                                 args.iter().map(|a| pp_immexpr(a, symbols))
                                 .collect::<Vec<String>>().join(", ")),

    If(cc, th, el, _) => format!("if {}:\n{}\nelse:\n{}",
                                 pp_immexpr(cc, symbols),
                                 pp_aexpr(th, symbols),
                                 pp_aexpr(el, symbols)),
  }
}

fn pp_immexpr<T>(expr: &ImmExpr<T>, symbols: &[String]) -> String {
  use self::ImmExpr::*;

  match expr {
    Number(n, _) => format!("{}", n),
    Id(n, _) => format!("{}", symbols[*n]),
    Bool(b, _) => format!("{}", b),
  }
}

// For these, we can directly implement Display
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
      BoolCheck   => write!(f, "bool_check"),
      IfCondCheck => write!(f, "if_cond_check"),
      User(n)     => write!(f, "{}", n),
    }
  }
}

impl Display for Prim1 {
  fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Prim1::*;

    match self {
      Not => write!(f, "!"),
    }
  }
}

impl Display for Prim2 {
   fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
    use self::Prim2::*;

    match self {
      Plus      => write!(f, "+"),
      Minus     => write!(f, "-"),
      Mult      => write!(f, "*"),
      And       => write!(f, "&&"),
      Or        => write!(f, "||"),
      Less      => write!(f, "<"),
      LessEq    => write!(f, "<="),
      Greater   => write!(f, ">"),
      GreaterEq => write!(f, ">="),
      Eq        => write!(f, "=="),
    }
  }
}
