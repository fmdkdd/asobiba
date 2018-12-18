use crate::lex::{BinOp, Keyword, TextPosition, Token, TokenKind, TokenStream};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parser
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Prim1 {
  Not,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Prim2 {
  Plus,
  Minus,
  Mult,
  And,
  Or,
  Greater,
  GreaterEq,
  Less,
  LessEq,
  Eq,
}

#[derive(Debug, PartialEq)]
pub enum Expr<T> {
  Number(i32, T),
  Id(usize, T),
  Bool(bool, T),
  Prim1(Prim1, Box<Expr<T>>, T),
  Prim2(Prim2, Box<Expr<T>>, Box<Expr<T>>, T),
  Apply(usize, Vec<Expr<T>>, T),
  Let(Vec<(usize, Expr<T>)>, Box<Expr<T>>, T),
  If(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>, T),
}

#[derive(Debug, PartialEq)]
pub struct Decl<T> {
  pub name: usize,
  pub args: Vec<usize>,
  pub body: Expr<T>,
  pub loc: Loc,
}

#[derive(Debug, PartialEq)]
pub struct Prog<T> {
  pub decls: Vec<Decl<T>>,
  pub body: Expr<T>,
}

#[derive(Debug)]
pub struct AST<T> {
  pub root: Prog<T>,
  pub symbols: Vec<String>,
}

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Loc {
  pub start: TextPosition,
  pub end: TextPosition,
}

impl Loc {
  fn new(start: TextPosition, end: TextPosition) -> Self {
    Loc {start, end}
  }
}

// Extract location info from EXPR.
pub fn loc(expr: &Expr<Loc>) -> &Loc {
  use self::Expr::*;
  match expr {
    Number(_, l)      => l,
    Id(_, l)          => l,
    Bool(_, l)        => l,
    Prim1(_, _, l)    => l,
    Prim2(_, _, _, l) => l,
    Apply(_, _, l)    => l,
    Let(_, _, l)      => l,
    If(_, _, _, l)    => l,
  }
}

// Helper for printing location of errors
fn pp_loc(t : &Token) -> String {
  format!("{}:{}", t.start.line, t.start.column)
}

fn expect(input: &mut TokenStream, kind: TokenKind) {
  let t = input.next();
  if t.kind != kind  {
    panic!("{}: Expected token '{:?}', but got '{:?}'",
           pp_loc(&t), kind, t.kind);
  }
}

pub fn parse(input: &mut TokenStream) -> AST<Loc> {
  let root = parse_prog(input);
  AST {
    root: root,
    symbols: input.symbols.clone(),
  }
}

// Have to change the Diamondback grammar to be LL(1) and handle (unspecified)
// operator precedence.  Let's go with standard precedence for binary operators.
//
// prog: (decl)* expr
// decl: 'def' IDENTIFIER '(' (id)* ')' ':' expr
// id: IDENTIFIER (, id)*
// expr: 'let' bindings 'in' expr
//     | 'if' expr ':' expr 'else:' expr
//     | equality
// equality: comparison ('==' equality)*
// comparison: addition (('<' | '<=' | '>' | '>=') addition)*
// addition: multiplication (('-' | '+') multiplication)*
// multiplication: unary ('*' unary)*
// unary: ('!') unary
//      | primary
// primary: NUMBER
//        | 'true' | 'false'
//        | '(' expr ')'
//        | IDENTIFIER
//        | IDENTIFIER '(' (expr (',' expr)*)? ')'
// bindings: IDENTIFIER '=' expr (',' bindings)*

fn parse_prog(input: &mut TokenStream) -> Prog<Loc> {

  let mut decls = Vec::new();

  while let TokenKind::Keyword(Keyword::Def) = input.peek().kind {
    decls.push(parse_decl(input));
  }

  let body = parse_expr(input);

  if !input.is_eof() {
    panic!("Expected end of file, got '{:?}'", input.peek().kind);
  }

  Prog { decls, body }
}

fn parse_decl(input: &mut TokenStream) -> Decl<Loc> {
  use self::TokenKind::*;

  let start = input.peek().start;
  expect(input, Keyword(self::Keyword::Def));

  let (name, _) = parse_ident(input);
  expect(input, LeftParen);

  let mut args = Vec::new();

  loop {
    match input.peek().kind {
      RightParen => break,

      _ => {
        args.push(parse_ident(input).0);

        if let Comma = input.peek().kind {
          input.next();
        }
      }
    }
  }

  expect(input, RightParen);
  expect(input, Colon);
  let body = parse_expr(input);
  let end = loc(&body).end;

  Decl { name, args, body, loc: Loc::new(start, end) }
}

fn parse_ident(input: &mut TokenStream) -> (usize, Loc) {
  let t = input.next();

  if let TokenKind::Ident(s) = t.kind {
    (s, Loc::new(t.start, t.end))
  } else {
    panic!("{}: Expected identifier, got {:?}", pp_loc(&t), t.kind);
  }
}

fn parse_expr(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::Keyword::*;

  match input.peek().kind {
    Keyword(Let) => {
      let t = input.next(); // eat the let

      let bindings = parse_bindings(input);
      expect(input, Keyword(In));
      let body = parse_expr(input);
      let end = loc(&body).end;
      Expr::Let(bindings, Box::new(body), Loc::new(t.start, end))
    }

    Keyword(If) => {
      let t = input.next(); // eat the if

      let cond = parse_expr(input);
      expect(input, Colon);
      let then = parse_expr(input);
      expect(input, Keyword(Else));
      expect(input, Colon);
      let els = parse_expr(input);
      let end = loc(&els).end;
      Expr::If(Box::new(cond), Box::new(then), Box::new(els),
               Loc::new(t.start, end))
    }

    _ => parse_equality(input)
  }
}

fn parse_bindings(input: &mut TokenStream) -> Vec<(usize, Expr<Loc>)> {
  let mut bindings = Vec::new();

  let s = parse_ident(input);
  expect(input, TokenKind::Equals);
  let expr = parse_expr(input);
  bindings.push((s.0, expr));

  if let TokenKind::Comma = input.peek().kind {
    input.next(); // eat the comma
    bindings.append(&mut parse_bindings(input));
  }
  bindings
}

fn parse_equality(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_comparison(input);

  loop {
    match input.peek().kind {
      BinOp(Eq) => {
        input.next(); // eat the operator

        let right = parse_comparison(input);
        let start = loc(&expr).start;
        let end = loc(&right).end;
        expr = Expr::Prim2(Prim2::Eq, Box::new(expr), Box::new(right),
                           Loc::new(start, end))
      }

      _ => break
    }
  }

  expr
}

fn parse_comparison(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_addition(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(op @ Less) | BinOp(op @ LessEq) |
      BinOp(op @ Greater) | BinOp(op @ GreaterEq) |
      BinOp(op @ Or) | BinOp(op @ And) => {
        input.next(); // eat the operator

        let right = parse_addition(input);
        let start = loc(&expr).start;
        let end = loc(&right).end;
        expr = Expr::Prim2(match op {
          Less      => Prim2::Less,
          LessEq    => Prim2::LessEq,
          Greater   => Prim2::Greater,
          GreaterEq => Prim2::GreaterEq,
          And       => Prim2::And,
          Or        => Prim2::Or,
          _ => unreachable!(),
        }, Box::new(expr), Box::new(right), Loc::new(start, end))
      }

      _ => break
    }
  }

  expr
}

fn parse_addition(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_multiplication(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(op @ Plus) | BinOp(op @ Minus) => {
        input.next(); // eat the operator

        let right = parse_multiplication(input);
        let start = loc(&expr).start;
        let end = loc(&right).end;
        expr = Expr::Prim2(match op {
          Plus => Prim2::Plus,
          Minus => Prim2::Minus,
          _ => unreachable!(),
        }, Box::new(expr), Box::new(right), Loc::new(start, end))
      }

      _ => break
    }
  }

  expr
}

fn parse_multiplication(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_unary(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(Mult) => {
        input.next(); // eat the operator

        let right = parse_unary(input);
        let start = loc(&expr).start;
        let end = loc(&right).end;
        expr = Expr::Prim2(Prim2::Mult, Box::new(expr), Box::new(right),
                           Loc::new(start, end))
      }

      _ => break
    }
  }

  expr
}

fn parse_unary(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;

  match input.peek().kind {
    Bang => {
      let t = input.next();
      let expr = parse_unary(input);
      let end = loc(&expr).end;
      Expr::Prim1(Prim1::Not, Box::new(expr), Loc::new(t.start, end))
    },
    _    => parse_primary(input),
  }
}

fn parse_primary(input: &mut TokenStream) -> Expr<Loc> {
  use self::TokenKind::*;
  use self::Keyword::*;
  use self::BinOp::*;

  let t = input.peek().clone();
  match t.kind {
    Number(n) => {
      let t = input.next();
      Expr::Number(n as i32, Loc::new(t.start, t.end))
    }

    Ident(s) => {
      let t = input.next();

      match input.peek().kind {
        LeftParen => {
          let args = parse_arglist(input);
          let end = args.last().map(|e| loc(&e).end).unwrap_or(t.end);
          Expr::Apply(s, args, Loc::new(t.start, end))
        }

        _ => Expr::Id(s, Loc::new(t.start, t.end))
      }
    }

    Keyword(True) => {
      let t = input.next();
      Expr::Bool(true, Loc::new(t.start, t.end))
    }

    Keyword(False) => {
      input.next();
      Expr::Bool(false, Loc::new(t.start, t.end))
    }

    LeftParen => {
      input.next(); // eat the paren
      let expr = parse_expr(input);
      expect(input, RightParen);
      expr
    }

    BinOp(Minus) => {
      let t = input.next();
      match input.peek().kind {
        Number(n) => {
          let nt = input.next();
          Expr::Number(-n as i32, Loc::new(t.start, nt.end))
        }
        _ => panic!("Expected a number, got end of input")
      }
    }

    _ => panic!("Expected an expression, got {:?}", t.kind)
  }
}

fn parse_arglist(input: &mut TokenStream) -> Vec<Expr<Loc>> {
  use self::TokenKind::*;

  expect(input, LeftParen);

  let mut exprs = Vec::new();

  loop {
    match input.peek().kind {
      RightParen => break,

      _ => {
        exprs.push(parse_expr(input));

        if let Comma = input.peek().kind {
          input.next();
        }
      }
    }
  }

  expect(input, RightParen);

  exprs
}

#[cfg(test)]
mod parse_tests {
  use super::*;

  fn locc(start_pos: u32, start_line: u32, start_column: u32,
          end_pos: u32, end_line: u32, end_column: u32) -> Loc {
    Loc::new(TextPosition {pos: start_pos, line: start_line, column: start_column},
             TextPosition {pos: end_pos, line: end_line, column: end_column})
  }

  fn loccc() -> Loc {
    locc(0, 0, 0, 0, 0, 0)
  }

  fn test_parser(input: &str, expected: &Prog<Loc>, symbols: &[&str]) {
    let mut t = TokenStream::new(input);
    let ast = parse(&mut t);

    assert!(t.is_eof());
    assert_eq!(t.symbols, symbols);
    assert_eq!(&ast.root, expected);
  }

  #[test]
  fn ifexpr() {
    use self::Expr::*;

    test_parser("if sub1(1): 6 else: 7",
                &Prog {
                  decls: vec![],
                  body: If(Box::new(Apply(0, vec![Number(1, locc(8, 1, 8, 8, 1, 8))],
                                          locc(3, 1, 3, 8, 1, 8))),
                           Box::new(Number(6, locc(12, 1, 12, 12, 1, 12))),
                           Box::new(Number(7, locc(20, 1, 20, 20, 1, 20))),
                           locc(0, 1, 0, 20, 1, 20)),
                },
                &["sub1"]);
  }

  #[test]
  fn letexpr() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("let a = 2 - 3 in
                 let b = 4 * 5 in
                 a + b",
                &Prog {
                  decls: vec![],
                  body: Let(vec![(0, Prim2(Minus,
                                           Box::new(Number(2, locc(8, 1, 8, 8, 1, 8))),
                                           Box::new(Number(3, locc(12, 1, 12, 12, 1, 12))),
                                           locc(8, 1, 8, 12, 1, 12)))],
                            Box::new(Let(vec![(1, Prim2(Mult,
                                                        Box::new(Number(4, locc(42, 2, 25, 42, 2, 25))),
                                                        Box::new(Number(5, locc(46, 2, 29, 46, 2, 29))),
                                                        locc(42, 2, 25, 46, 2, 29)))],
                                         Box::new(Prim2(Plus,
                                                        Box::new(Id(0, locc(68, 3, 17, 68, 3, 17))),
                                                        Box::new(Id(1, locc(72, 3, 21, 72, 3, 21))),
                                                        locc(68, 3, 17, 72, 3, 21))),
                                         locc(34, 2, 17, 72, 3, 21))),
                            locc(0, 1, 0, 72, 3, 21))
                },
                &["a", "b"]);
  }

  #[test]
  fn parenexpr() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("2 + (3 + 4)",
                &Prog {
                  decls: vec![],
                  body: Prim2(Plus,
                              Box::new(Number(2, locc(0, 1, 0, 0, 1, 0))),
                              Box::new(Prim2(Plus,
                                             Box::new(Number(3, locc(5, 1, 5, 5, 1, 5))),
                                             Box::new(Number(4, locc(9, 1, 9, 9, 1, 9))),
                                             locc(5, 1, 5, 9, 1, 9))),
                              locc(0, 1, 0, 9, 1, 9))
                },
                &[]);
  }

  #[test]
  fn bool_expr() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("1<2<=3 == false",
                &Prog {
                  decls: vec![],
                  body: Prim2(Eq,
                              Box::new(Prim2(LessEq,
                                             Box::new(Prim2(Less,
                                                            Box::new(Number(1, locc(0, 1, 0, 0, 1, 0))),
                                                            Box::new(Number(2, locc(2, 1, 2, 2, 1, 2))),
                                                            locc(0, 1, 0, 2, 1, 2))),
                                             Box::new(Number(3, locc(5, 1, 5, 5, 1, 5))),
                                             locc(0, 1, 0, 5, 1, 5))),
                              Box::new(Bool(false, locc(10, 1, 10, 14, 1, 14))),
                              locc(0, 1, 0, 14, 1, 14))
                },
                &[]);
  }

  #[test]
  fn bool_expr2() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("true || false",
                &Prog {
                  decls: vec![],
                  body: Prim2(Or,
                              Box::new(Bool(true, locc(0, 1, 0, 3, 1, 3))),
                              Box::new(Bool(false, locc(8, 1, 8, 12, 1, 12))),
                              locc(0, 1, 0, 12, 1, 12))
                },
                &[]);
  }

  #[test]
  fn neg_num() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("2 * -1",
                &Prog {
                  decls: vec![],
                  body: Prim2(Mult,
                              Box::new(Number(2, locc(0, 1, 0, 0, 1, 0))),
                              Box::new(Number(-1, locc(4, 1, 4, 5, 1, 5))),
                              locc(0, 1, 0, 5, 1, 5))
                },
                &[]);
  }

  #[test]
  fn neg_num2() {
    use self::Expr::*;

    test_parser("-1",
                &Prog { decls: vec![], body: Number(-1, locc(0, 1, 0, 1, 1, 1)) },
                &[]);
  }

  #[test]
  fn neg_num3() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("1--1",
                &Prog {
                  decls: vec![],
                  body: Prim2(Minus,
                              Box::new(Number(1, locc(0, 1, 0, 0, 1, 0))),
                              Box::new(Number(-1, locc(2, 1, 2, 3, 1, 3))),
                              locc(0, 1, 0, 3, 1, 3))
                },
                &[]);
  }

  #[test]
  fn neg_num4() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("-1--1",
                &Prog {
                  decls: vec![],
                  body: Prim2(Minus,
                              Box::new(Number(-1, locc(0, 1, 0, 1, 1, 1))),
                              Box::new(Number(-1, locc(3, 1, 3, 4, 1, 4))),
                              locc(0, 1, 0, 4, 1, 4))
                },
                &[]);
  }

  #[test]
  fn decl() {
    use self::Expr::*;

    test_parser("def foo(): 1
                 foo()",
                &Prog {
                  decls: vec![Decl { name: 0, args: vec![],
                                     body: Number(1, locc(11, 1, 11, 11, 1, 11)),
                                     loc: locc(0, 1, 0, 11, 1, 11) }],
                  body: Apply(0, vec![], locc(30, 2, 17, 32, 2, 19))
                },
                &["foo"]);
  }

    #[test]
  fn decl2() {
    use self::Expr::*;

    test_parser("def foo(a,b): 1
                 foo(2,3)",
                &Prog {
                  decls: vec![Decl { name: 0, args: vec![1,2],
                                     body: Number(1, locc(14, 1, 14, 14, 1, 14)),
                                     loc: locc(0, 1, 0, 14, 1, 14) }],
                  body: Apply(0, vec![Number(2, locc(37, 2, 21, 37, 2, 21)),
                                      Number(3, locc(39, 2, 23, 39, 2, 23))],
                              locc(33, 2, 17, 39, 2, 23))
                },
                &["foo", "a", "b"]);
  }
}
