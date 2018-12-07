use crate::lex::{BinOp, Keyword, Token, TokenKind, TokenStream};

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

pub fn parse(input: &mut TokenStream) -> AST<()> {
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

fn parse_prog(input: &mut TokenStream) -> Prog<()> {

  let mut decls = Vec::new();

  while let TokenKind::Keyword(Keyword::Def) = input.peek().kind {
    input.next(); // eat the Def
    decls.push(parse_decl(input));
  }

  let body = parse_expr(input);

  Prog { decls, body }
}

fn parse_decl(input: &mut TokenStream) -> Decl<()> {
  use self::TokenKind::*;

  let name = parse_ident(input);
  expect(input, TokenKind::LeftParen);

  let mut args = Vec::new();

  loop {
    match input.peek().kind {
      RightParen => break,

      _ => {
        args.push(parse_ident(input));

        if let Comma = input.peek().kind {
          input.next();
        }
      }
    }
  }

  expect(input, RightParen);
  expect(input, TokenKind::Colon);
  let body = parse_expr(input);

  Decl { name, args, body }
}

fn parse_ident(input: &mut TokenStream) -> usize {
  let t = input.next();

  if let TokenKind::Ident(s) = t.kind {
    s
  } else {
    panic!("{}: Expected identifier, got {:?}", loc(&t), t.kind);
  }
}

fn parse_expr(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::Keyword::*;

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

    _ => parse_equality(input)
  }
}

fn parse_bindings(input: &mut TokenStream) -> Vec<(usize, Expr<()>)> {
  let mut bindings = Vec::new();

  let s = parse_ident(input);
  expect(input, TokenKind::Equals);
  let expr = parse_expr(input);
  bindings.push((s, expr));

  if let TokenKind::Comma = input.peek().kind {
    input.next(); // eat the comma
    bindings.append(&mut parse_bindings(input));
  }
  bindings
}

fn parse_equality(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_comparison(input);

  loop {
    match input.peek().kind {
      BinOp(Eq) => {
        input.next(); // eat the operator

        let right = parse_comparison(input);
        expr = Expr::Prim2(Prim2::Eq, Box::new(expr), Box::new(right), ())
      }

      _ => break
    }
  }

  expr
}

fn parse_comparison(input: &mut TokenStream) -> Expr<()> {
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
          expr = Expr::Prim2(match op {
            Less      => Prim2::Less,
            LessEq    => Prim2::LessEq,
            Greater   => Prim2::Greater,
            GreaterEq => Prim2::GreaterEq,
            And       => Prim2::And,
            Or        => Prim2::Or,
            _ => unreachable!(),
          }, Box::new(expr), Box::new(right), ())
      }

      _ => break
    }
  }

  expr
}

fn parse_addition(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::BinOp::*;

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
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_unary(input);

  loop {
    match input.peek().kind.clone() {
      BinOp(Mult) => {
        input.next(); // eat the operator

        let right = parse_unary(input);
        expr = Expr::Prim2(Prim2::Mult, Box::new(expr), Box::new(right), ())
      }

      _ => break
    }
  }

  expr
}

fn parse_unary(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;

  match input.peek().kind {
    Bang => { input.next(); Expr::Prim1(Prim1::Not, Box::new(parse_unary(input)), ()) },
    _    => parse_primary(input),
  }
}

fn parse_primary(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::Keyword::*;
  use self::BinOp::*;

  let t = input.peek().clone();
  match t.kind {
    Number(n) => { input.next(); Expr::Number(n as i32, ()) }

    Ident(s) => {
      input.next();

      match input.peek().kind {
        LeftParen => {
          let args = parse_arglist(input);
          Expr::Apply(s, args, ())
        }

        _ => Expr::Id(s, ())
      }
    }

    Keyword(True) => {
      input.next();
      Expr::Bool(true, ())
    }

    Keyword(False) => {
      input.next();
      Expr::Bool(false, ())
    }

    LeftParen => {
      input.next(); // eat the paren
      let expr = parse_expr(input);
      expect(input, RightParen);
      expr
    }

    BinOp(Minus) => {
      input.next();
      match input.peek().kind {
        Number(n) => { input.next(); Expr::Number(-n as i32, ()) }
        _ => panic!("Expected a number, got end of input")
      }
    }

    _ => unreachable!()
  }
}

fn parse_arglist(input: &mut TokenStream) -> Vec<Expr<()>> {
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

  fn test_parser(input: &str, expected: &Prog<()>, symbols: &[&str]) {
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
                  body: If(Box::new(Apply(0, vec![Number(1, ())], ())),
                           Box::new(Number(6, ())),
                           Box::new(Number(7, ())), ()),
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
                  body: Let(vec![(0, Prim2(Minus, Box::new(Number(2, ())), Box::new(Number(3, ())), ()))],
                            Box::new(Let(vec![(1, Prim2(Mult, Box::new(Number(4, ())), Box::new(Number(5, ())), ()))],
                                         Box::new(Prim2(Plus, Box::new(Id(0, ())), Box::new(Id(1, ())), ())), ())), ())
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
                              Box::new(Number(2, ())),
                              Box::new(Prim2(Plus, Box::new(Number(3, ())), Box::new(Number(4, ())), ())), ())
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
                                                            Box::new(Number(1, ())),
                                                            Box::new(Number(2, ())), ())),
                                             Box::new(Number(3, ())), ())),
                              Box::new(Bool(false, ())), ())
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
                              Box::new(Bool(true, ())),
                              Box::new(Bool(false, ())), ())
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
                              Box::new(Number(2, ())),
                              Box::new(Number(-1, ())), ())
                },
                &[]);
  }

  #[test]
  fn neg_num2() {
    use self::Expr::*;

    test_parser("-1",
                &Prog { decls: vec![], body: Number(-1, ()) },
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
                              Box::new(Number(1, ())),
                              Box::new(Number(-1, ())), ())
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
                              Box::new(Number(-1, ())),
                              Box::new(Number(-1, ())), ())
                },
                &[]);
  }

  #[test]
  fn decl() {
    use self::Expr::*;

    test_parser("def foo(): 1
                 foo()",
                &Prog {
                  decls: vec![Decl { name: 0, args: vec![], body: Number(1, ()) }],
                  body: Apply(0, vec![], ())
                },
                &["foo"]);
  }

    #[test]
  fn decl2() {
    use self::Expr::*;

    test_parser("def foo(a,b): 1
                 foo(2,3)",
                &Prog {
                  decls: vec![Decl { name: 0, args: vec![1,2], body: Number(1, ()) }],
                  body: Apply(0, vec![Number(2, ()), Number(3, ())], ())
                },
                &["foo", "a", "b"]);
  }
}
