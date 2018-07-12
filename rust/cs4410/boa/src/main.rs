// The Boa language is closer to ML than Lisp, so we discard the S-exp parser.

use std::io::{self, Read};
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Character stream
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// This is only to record position, line and column information when building
// the tokens

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct TextPosition {
  pub pos: u32,                     // absolute character position
  pub line: u32,                    // line number (starts at 1)
  pub column: u32,                  // column number (starts at 0)
}

impl TextPosition {
  pub fn new() -> Self {
    TextPosition { pos: 0, line: 1, column: 0 }
  }
}

struct CharStream<'a> {
  input: Peekable<Chars<'a>>,
  pos_of_current_char: Option<TextPosition>,
  pos_of_next_char: TextPosition,
}

impl<'a> CharStream<'a> {
  fn new(input: &'a str) -> Self {
    CharStream {
      input: input.chars().peekable(),
      pos_of_current_char: None,
      pos_of_next_char: TextPosition::new(),
    }
  }

  fn peek(&mut self) -> Option<char> {
    self.input.peek().map(|c| *c)
  }

  fn next(&mut self) -> Option<char> {
    self.pos_of_current_char = Some(self.pos_of_next_char);
    let c = self.input.next();
    self.pos_of_next_char.pos += 1;
    self.pos_of_next_char.column += 1;
    if let Some('\n') = c {
      self.pos_of_next_char.line += 1;
      self.pos_of_next_char.column = 0;
    }
    c
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tokenizer
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
  kind: TokenKind,
  start: TextPosition,
  end: TextPosition,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
  EOF,
  LeftParen,
  RightParen,
  Colon,
  Comma,
  Equals,
  BinOp(BinOp),
  Keyword(Keyword),
  Ident(usize),
  Number(i64),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
  Let,
  In,
  If,
  Else,
  Add1,
  Sub1,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinOp {
  Plus,
  Minus,
  Mult,
}

pub struct TokenStream<'a> {
  input: CharStream<'a>,
  peeked_token: Option<Token>,
  saved_position: Option<TextPosition>,
  symbols: Vec<String>,
}

impl<'a> TokenStream<'a> {
  pub fn new(input: &'a str) -> Self {
    TokenStream {
      input: CharStream::new(input),
      peeked_token: None,
      saved_position: None,
      symbols: Vec::new(),
    }
  }

  // Save position of the *next* character
  fn save_position(&mut self) {
    self.saved_position = Some(self.input.pos_of_next_char);
  }

  // Emit token with the position of the *current* character as the end position
  fn emit(&mut self, kind: TokenKind) -> Token {
    let end = self.input.pos_of_current_char.unwrap();

    let start = match self.saved_position {
      None    => end,
      Some(p) => p,
    };
    self.saved_position = None;

    Token { kind, start, end }
  }

  fn eat_space(&mut self) {
    loop {
      match self.input.peek() {
        None                          => return,
        Some(c) if !c.is_whitespace() => return,
        _                             => self.input.next(),
      };
    }
  }

  fn eat_comment(&mut self) {
    if let Some('#') = self.input.peek() {
      self.input.next();
      loop {
        match self.input.peek() {
          None       => return,
          Some('\n') => { self.input.next(); return },
          _          => self.input.next(),
        };
      }
    }
  }

  fn read_number(&mut self) -> i64 {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                       => break,
        Some(c) if !c.is_digit(10) => break,
        Some(_)                    => s.push(self.input.next().unwrap()),
      }
    }
    let pos = self.input.pos_of_next_char;
    s.parse::<i64>().expect(&format!("{}:{}: Failed to parse decimal number: '{}'",
                                     pos.line, pos.column, s))
  }

  fn read_word(&mut self) -> TokenKind {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => break,
        Some(c) if c.is_whitespace() => break,
        Some('(') | Some(')') | Some(',')
          | Some(':') | Some('=') | Some('+')
          | Some('-') | Some('*')  => break,
        Some(_)                      => s.push(self.input.next().unwrap()),
      }
    }

    // It's either a keyword or a plain identifier
    match s.as_str() {
      "let" => TokenKind::Keyword(Keyword::Let),
      "in" => TokenKind::Keyword(Keyword::In),
      "if" => TokenKind::Keyword(Keyword::If),
      "else" => TokenKind::Keyword(Keyword::Else),
      "add1" => TokenKind::Keyword(Keyword::Add1),
      "sub1" => TokenKind::Keyword(Keyword::Sub1),

      _ => TokenKind::Ident(
        match self.symbols.iter().position(|sym| s == *sym) {
          Some(idx) => idx,
          None => {
            self.symbols.push(s);
            self.symbols.len() - 1
          }
        })
    }
  }

  // Consume input until we have eaten another token
  fn advance(&mut self) -> Token {
    loop {
      match self.input.peek() {
        // Skip whitespace
        Some(c) if c.is_whitespace() => self.eat_space(),
        // Skip comments
        Some('#')                    => self.eat_comment(),
        _                            => break,
      }
    }

    // Bail if no more input
    if let None = self.input.peek() {
      return self.emit(TokenKind::EOF)
    }

    match self.input.peek().unwrap() {
      '('                 => { self.input.next(); self.emit(TokenKind::LeftParen) },
      ')'                 => { self.input.next(); self.emit(TokenKind::RightParen) },
      ','                 => { self.input.next(); self.emit(TokenKind::Comma) },
      ':'                 => { self.input.next(); self.emit(TokenKind::Colon) },
      '='                 => { self.input.next(); self.emit(TokenKind::Equals) },
      '+'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Plus)) },
      '-'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Minus)) },
      '*'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Mult)) },
      c if c.is_digit(10) => { self.save_position();
                               let n = self.read_number();
                               self.emit(TokenKind::Number(n)) },
      _                   => { self.save_position();
                               let t = self.read_word();
                               self.emit(t) },
    }
  }

  pub fn peek(&mut self) -> &Token {
    if self.peeked_token.is_none() {
      self.peeked_token = Some(self.advance());
    }
    &self.peeked_token.as_ref().unwrap()
  }

  pub fn next(&mut self) -> Token {
    if self.peeked_token.is_some() {
      let ret = self.peeked_token.clone().unwrap();
      self.peeked_token = None;
      ret
    } else {
      self.advance()
    }
  }

  pub fn is_eof(&mut self) -> bool {
    self.peek().kind == TokenKind::EOF
  }
}

#[cfg(test)]
mod lex_tests {
  use super::*;

  fn test_lexer(input: &str, expected: &[TokenKind], symbols: &[&str]) {
    let mut t = TokenStream::new(input);

    for token in expected {
      assert_eq!(*token, t.next().kind);
    }
    assert!(t.is_eof());

    assert_eq!(t.symbols, symbols);
  }

  #[test]
  fn ifexpr() {
    use TokenKind::*;
    use Keyword::*;

    let tokens = [
      Keyword(If),
      Keyword(Sub1),
      LeftParen,
      Number(1),
      RightParen,
      Colon,
      Number(6),
      Keyword(Else),
      Colon,
      Number(7),
      EOF];

    test_lexer("if sub1(1): 6 else: 7", &tokens, &[]);
  }

  #[test]
  fn numexpr() {
    use TokenKind::*;
    use BinOp::*;

    let tokens = [
      LeftParen,
      Number(2),
      BinOp(Minus),
      Number(3),
      RightParen,
      BinOp(Plus),
      LeftParen,
      Number(4),
      BinOp(Mult),
      Number(5),
      RightParen,
      EOF];
    test_lexer("(2 - 3) + (4 * 5)", &tokens, &[]);
  }

  #[test]
  fn letexpr() {
    use TokenKind::*;
    use Keyword::*;
    use BinOp::*;

    let symbols = [
      "first", "second"
    ];

    let tokens = [
      Keyword(Let),
      Ident(0),
      Equals,
      Number(2),
      BinOp(Minus),
      Number(3),
      Keyword(In),
      Keyword(Let),
      Ident(1),
      Equals,
      Number(4),
      BinOp(Mult),
      Number(5),
      Keyword(In),
      Ident(0),
      BinOp(Plus),
      Ident(1),
      EOF];
    test_lexer("let first = 2 - 3 in
                let second = 4 * 5 in
                first + second", &tokens, &symbols);
  }

  #[test]
  fn comment() {
    use TokenKind::*;

    let tokens = [
      Number(2),
      Number(3),
    ];
    test_lexer("# ignored
2
# ignored
3 # + 2", &tokens, &vec![]);
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parser
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, PartialEq)]
enum Prim1 {
  Add1,
  Sub1,
}

#[derive(Debug, PartialEq)]
enum Prim2 {
  Plus,
  Minus,
  Mult,
}

#[derive(Debug, PartialEq)]
enum Expr<T> {
  Number(i32, T),
  Id(usize, T),
  Prim1(Prim1, Box<Expr<T>>, T),
  Prim2(Prim2, Box<Expr<T>>, Box<Expr<T>>, T),
  Let(Vec<(usize, Expr<T>)>, Box<Expr<T>>, T),
  If(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>, T),
}

#[derive(Debug)]
struct AST<T> {
  root: Expr<T>,
  symbols: Vec<String>,
}

// Helper for printing location of errors
fn loc(t : &Token) -> String {
  format!("{}:{}", t.start.line, t.start.column)
}

fn expect(input: &mut TokenStream, kind: TokenKind) {
  let t = input.next();
  if t.kind != kind  {
    panic!("{}: Expected token '{:?}', but got '{:?}'",
           loc(&t), kind, t.kind);
  }
}

fn parse(input: &mut TokenStream) -> AST<()> {
  let root = parse_expr(input);
  AST {
    root: root,
    symbols: input.symbols.clone(),
  }
}

// Have to change the Boa grammar to be LL(1) and handle (unspecified) operator
// precedence.  Let's go with standard precedence for binary operators.
//
// expr: 'let' bindings 'in' expr
//     | 'if' expr ':' expr 'else:' expr
//     | addition
// addition: multiplication (('-' | '+') multiplication)*
// multiplication: primary ('*' primary)*
// primary: NUMBER | IDENTIFIER
//        | ('add1' | 'sub1') '(' expr ')'
//        | '(' expr ')'
// bindings: IDENTIFIER '=' expr (',' bindings)*

fn parse_expr(input: &mut TokenStream) -> Expr<()> {
  use TokenKind::*;
  use Keyword::*;

  match input.peek().kind {
    Keyword(Let) => {
      input.next(); // eat the let

      let bindings = parse_bindings(input);
      expect(input, Keyword(In));
      let body = parse_expr(input);
      Expr::Let(bindings, Box::new(body), ())
    }

    Keyword(If) => {
      input.next(); // eat the if

      let cond = parse_expr(input);
      expect(input, Colon);
      let then = parse_expr(input);
      expect(input, Keyword(Else));
      expect(input, Colon);
      let els = parse_expr(input);
      Expr::If(Box::new(cond), Box::new(then), Box::new(els), ())
    }

    _ => parse_addition(input)
  }
}

fn parse_bindings(input: &mut TokenStream) -> Vec<(usize, Expr<()>)> {
  let t = input.next();
  let mut bindings = Vec::new();

  if let TokenKind::Ident(s) = t.kind {
    expect(input, TokenKind::Equals);
    let expr = parse_expr(input);
    bindings.push((s, expr));

    if let TokenKind::Comma = input.peek().kind {
      input.next(); // eat the comma
      bindings.append(&mut parse_bindings(input));
    }
    bindings
  } else {
    panic!("{}: Expected identifier, got {:?}", loc(&t), t.kind);
  }
}

fn parse_addition(input: &mut TokenStream) -> Expr<()> {
  use TokenKind::*;
  use BinOp::*;

  let mut expr = parse_multiplication(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(op @ Plus) | BinOp(op @ Minus) => {
        input.next(); // eat the operator

        let right = parse_multiplication(input);
        expr = Expr::Prim2(match op {
          Plus => Prim2::Plus,
          Minus => Prim2::Minus,
          _ => unreachable!(),
        }, Box::new(expr), Box::new(right), ())
      }

      _ => break
    }
  }

  expr
}

fn parse_multiplication(input: &mut TokenStream) -> Expr<()> {
  use TokenKind::*;
  use BinOp::*;

  let mut expr = parse_primary(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(Mult) => {
        input.next(); // eat the operator

        let right = parse_primary(input);
        expr = Expr::Prim2(Prim2::Mult, Box::new(expr), Box::new(right), ())
      }

      _ => break
    }
  }

  expr
}

fn parse_primary(input: &mut TokenStream) -> Expr<()> {
  use TokenKind::*;
  use Keyword::*;

  let t = input.peek().clone();
  match t.kind {
    Number(n) => { input.next(); Expr::Number(n as i32, ()) }

    Ident(s) => { input.next(); Expr::Id(s, ()) }

    Keyword(ref k @ Add1) | Keyword(ref k @ Sub1)=> {
      input.next(); // eat the keyword
      expect(input, LeftParen);
      let expr = parse_expr(input);
      expect(input, RightParen);
      Expr::Prim1(match k {
        Add1 => Prim1::Add1,
        Sub1 => Prim1::Sub1,
        _ => unreachable!(),
      }, Box::new(expr), ())
    }

    LeftParen => {
      input.next(); // eat the paren
      let expr = parse_expr(input);
      expect(input, RightParen);
      expr
    }

    _ => unreachable!()
  }
}

#[cfg(test)]
mod parse_tests {
  use super::*;

  fn test_parser(input: &str, expected: &Expr<()>, symbols: &[&str]) {
    let mut t = TokenStream::new(input);
    let ast = parse(&mut t);

    assert!(t.is_eof());
    assert_eq!(t.symbols, symbols);
    assert_eq!(&ast.root, expected);
  }

  #[test]
  fn ifexpr() {
    use Expr::*;
    use Prim1::*;

    test_parser("if sub1(1): 6 else: 7",
                &If(Box::new(Prim1(Sub1, Box::new(Number(1, ())), ())),
                    Box::new(Number(6, ())),
                    Box::new(Number(7, ())), ()),
                &[]);
  }

  #[test]
  fn letexpr() {
    use Expr::*;
    use Prim2::*;

    test_parser("let a = 2 - 3 in
                 let b = 4 * 5 in
                 a + b",
                &Let(vec![(0, Prim2(Minus, Box::new(Number(2, ())), Box::new(Number(3, ())), ()))],
                     Box::new(Let(vec![(1, Prim2(Mult, Box::new(Number(4, ())), Box::new(Number(5, ())), ()))],
                                  Box::new(Prim2(Plus, Box::new(Id(0, ())), Box::new(Id(1, ())), ())), ())), ()),
                &["a", "b"]);
  }

  #[test]
  fn parenexpr() {
    use Expr::*;
    use Prim2::*;

    test_parser("2 + (3 + 4)",
                &Prim2(Plus,
                       Box::new(Number(2, ())),
                       Box::new(Prim2(Plus, Box::new(Number(3, ())), Box::new(Number(4, ())), ())), ()),
                &[]);
  }
}


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
  use Expr::*;

  match expr {
    Number(n, t) => (Number(n, (seed, t)), seed+1),
    Id(s, t) => (Id(s, (seed, t)), seed+1),
    Prim1(op, exp, t) => {
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
  use Expr::*;

  match expr {
    Number(_, t) => t,
    Id(_, t) => t,
    Prim1(_, _, t) => t,
    Prim2(_, _, _, t) => t,
    Let(_, _, t) => t,
    If(_, _, _, t) => t,
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

fn compile<T>(ast: &AST<(usize, T)>) -> Vec<Instr> {
  compile_expr(&ast.root, &ast.symbols, &mut vec![])
}

fn compile_expr<T>(e: &Expr<(usize, T)>, symbols: &[String], env: &Vec<usize>) -> Vec<Instr> {
  use Instr::*;
  use Prim1::*;
  use Prim2::*;
  use Arg::*;
  use Reg::*;
  use Expr::*;

  match e {
    Number(n, _) => vec![Mov(Reg(EAX), Const(*n))],

    Id(s, _) => match lookup(*s, env) {
      Some(n) => vec![Mov(Reg(EAX), RegOffset(ESP, n))],
      None => panic!("Identifier not bound '{}'", symbols[*s]),
    }

    Prim1(Add1, ex, _) => {
      let mut v = compile_expr(ex, symbols,env);
      v.push(Inc(Reg(EAX)));
      v
    }

    Prim1(Sub1, ex, _) => {
      let mut v = compile_expr(ex, symbols, env);
      v.push(Dec(Reg(EAX)));
      v
    }

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
      v.push(match op {
        Plus => Add(a, b),
        Minus => Sub(a, b),
        Mult => IMul(a, b),
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
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Reg::*;

    match self {
      EAX => write!(f, "eax"),
      ESP => write!(f, "esp"),
    }
  }
}

impl Display for Arg {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Arg::*;

    match self {
      Const(n) => write!(f, "{}", n),
      Reg(r) => write!(f, "{}", r),
      RegOffset(r, o) => write!(f, "[{} - 4*{}]", r, o),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    use Instr::*;

    match self {
      Mov(dst, src) => writeln!(f, "  mov {}, {}", dst, src),
      Inc(dst) => writeln!(f, "  inc {}", dst),
      Dec(dst) => writeln!(f, "  dec {}", dst),
      Add(dst, src) => writeln!(f, "  add {}, {}", dst, src),
      Sub(dst, src) => writeln!(f, "  sub {}, {}", dst, src),
      IMul(dst, src) => writeln!(f, "  imul {}, {}", dst, src),
      Cmp(a, b) => writeln!(f, "  cmp {}, {}", a, b),
      Label(s) => writeln!(f, "{}:", s),
      Jmp(s) => writeln!(f, "  jmp {}", s),
      Je(s) => writeln!(f, "  je {}", s),
    }
  }
}

// Binary expressions of arbitrary size cannot be compiled directly to return to
// EAX.  We transform the program into an Administrative Normal Form (ANF),
// which is composed only of *immediate* expressions which can be trivially
// compiled.

fn is_anf<T>(expr: &Expr<T>) -> bool {
  use Expr::*;

  match expr {
    Prim1(_, e, _) => is_anf(e),
    Prim2(_, l, r, _) => is_imm(l) && is_imm(r),
    Let(es, e, _) => es.iter().all(|(_,e)| is_anf(e)) && is_anf(e),
    If(e1, e2, e3, _) => is_imm(e1) && is_anf(e2) && is_anf(e3),
    e => is_imm(e),
  }
}

fn is_imm<T>(expr: &Expr<T>) -> bool {
  use Expr::*;
  match expr {
    Number(_,_) | Id(_,_) => true,
    _ => false
  }
}

// This helper will decompose an expression of arbitrary depth into ANF, where
// all immediate expressions are put in a single context (second returned arg).
// After that, into_anf will simply create a single Let expr with this context
// as bindings.
fn into_anf1<T>(expr: Expr<(usize, T)>, symbols: &mut Vec<String>)
                -> (Expr<()>, Vec<(usize, Expr<()>)>) {
  use Expr::*;

  match expr {
    Number(n, _) => (Number(n, ()), vec![]),
    Id(s, _) => (Id(s, ()), vec![]),
    Prim1(p, e, _) => {
      let (imm, mut ctx) = into_anf1(*e, symbols);
      (Prim1(p, Box::new(imm), ()), ctx)
    },

    Prim2(p, l, r, (t, _)) => {
      let (l_imm, mut l_ctx) = into_anf1(*l, symbols);
      let (r_imm, mut r_ctx) = into_anf1(*r, symbols);
      let sym = format!("prim2_{}", t);
      symbols.push(sym);
      let s = symbols.len() - 1;
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
    Let(bs, e, _) => {
      let mut ctx = vec![];
      for (x,b) in bs {
        let (imm, mut c) = into_anf1(b, symbols);
        ctx.append(&mut c);
        ctx.push((x, imm));
      }
      let (e_anf, mut c) = into_anf1(*e, symbols);
      ctx.append(&mut c);
      (Let(ctx, Box::new(e_anf), ()), vec![])
    }

    If(cc, th, el, _) => {
      let (cc_anf, ctx) = into_anf1(*cc, symbols);
      let (th_anf, th_ctx) = into_anf1(*th, symbols);
      let (el_anf, el_ctx) = into_anf1(*el, symbols);
      (If(Box::new(cc_anf),
          Box::new(Let(th_ctx, Box::new(th_anf), ())),
          Box::new(Let(el_ctx, Box::new(el_anf), ())),
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
    Expr::Let(ctx, Box::new(e), ())
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
  use Expr::*;
  use Prim2::*;

  match expr {
    Number(n, _) => format!("{}", n),
    Id(n, _) => format!("{}", symbols[*n]),
    Prim1(p, e, _) => format!("{:?}({})", p, pp_expr(e, symbols)),
    Prim2(p, l, r, _) => format!("{} {} {}",
                                 pp_expr(l, symbols),
                                 match p {
                                   Plus => "+",
                                   Minus => "-",
                                   Mult => "*",
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



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Main
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fn main() {
  let stdin = io::stdin();
  let mut input = String::new();
  stdin.lock().read_to_string(&mut input).unwrap();
  let ast = parse(&mut TokenStream::new(&input));

  // First we tag each node with a unique number.  This is needed for compiling
  // IF and for transforming into ANF.  Then we reduce to ANF in order to
  // compile binary expressions.
  let anf_ast = into_anf(number(ast));

  // println!("{}", pp(&anf_ast));

  // Then we emit a list of assembly instructions (we renumber since the ANF
  // transformtion lost the numerotation)
  let instrs = compile(&number(anf_ast));
  // And finally we emit ASM as a string
  let asm = emit_asm(&instrs);
  // And we print the result!
  println!("{}", asm);
}
