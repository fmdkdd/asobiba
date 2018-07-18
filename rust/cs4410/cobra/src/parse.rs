use lex::{BinOp, Keyword, Token, TokenKind, TokenStream};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parser
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// The Boa language is closer to ML than Lisp, so we discard the S-exp parser.

#[derive(Debug, PartialEq)]
pub enum Prim1 {
  Add1,
  Sub1,
  Print,
  IsBool,
  IsNum,
  Not,
}

#[derive(Debug, PartialEq)]
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
  Let(Vec<(usize, Expr<T>)>, Box<Expr<T>>, T),
  If(Box<Expr<T>>, Box<Expr<T>>, Box<Expr<T>>, T),
}

#[derive(Debug)]
pub struct AST<T> {
  pub root: Expr<T>,
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
  let root = parse_expr(input);
  AST {
    root: root,
    symbols: input.symbols.clone(),
  }
}

// Have to change the Cobra grammar to be LL(1) and handle (unspecified)
// operator precedence.  Let's go with standard precedence for binary operators.
//
// expr: 'let' bindings 'in' expr
//     | 'if' expr ':' expr 'else:' expr
//     | equality
// equality: comparison ('==' equality)*
// comparison: addition (('<' | '<=' | '>' | '>=') addition)*
// addition: multiplication (('-' | '+') multiplication)*
// multiplication: unary ('*' unary)*
// unary: ('!') unary
//      | primary
// primary: NUMBER | IDENTIFIER
//        | 'true' | 'false'
//        | ('add1' | 'sub1' | 'print' | 'isbool' | 'isnum') '(' expr ')'
//        | '(' expr ')'
// bindings: IDENTIFIER '=' expr (',' bindings)*

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

fn parse_equality(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::BinOp::*;

  let mut expr = parse_comparison(input);

  loop {
    match input.peek().kind.clone() {
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
    Bang => Expr::Prim1(Prim1::Not, Box::new(parse_unary(input)), ()),
    _    => parse_primary(input),
  }
}

fn parse_primary(input: &mut TokenStream) -> Expr<()> {
  use self::TokenKind::*;
  use self::Keyword::*;

  let t = input.peek().clone();
  match t.kind {
    Number(n) => { input.next(); Expr::Number(n as i32, ()) }

    Ident(s) => { input.next(); Expr::Id(s, ()) }

    Keyword(True) => {
      input.next();
      Expr::Bool(true, ())
    }

    Keyword(False) => {
      input.next();
      Expr::Bool(false, ())
    }

    Keyword(ref k @ Add1) |
    Keyword(ref k @ Sub1) |
    Keyword(ref k @ Print) |
    Keyword(ref k @ IsBool) |
    Keyword(ref k @ IsNum) => {
      input.next(); // eat the keyword
      expect(input, LeftParen);
      let expr = parse_expr(input);
      expect(input, RightParen);
      Expr::Prim1(match k {
        Add1   => Prim1::Add1,
        Sub1   => Prim1::Sub1,
        Print  => Prim1::Print,
        IsBool => Prim1::IsBool,
        IsNum  => Prim1::IsNum,
        _      => unreachable!(),
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
    use self::Expr::*;
    use self::Prim1::*;

    test_parser("if sub1(1): 6 else: 7",
                &If(Box::new(Prim1(Sub1, Box::new(Number(1, ())), ())),
                    Box::new(Number(6, ())),
                    Box::new(Number(7, ())), ()),
                &[]);
  }

  #[test]
  fn letexpr() {
    use self::Expr::*;
    use self::Prim2::*;

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
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("2 + (3 + 4)",
                &Prim2(Plus,
                       Box::new(Number(2, ())),
                       Box::new(Prim2(Plus, Box::new(Number(3, ())), Box::new(Number(4, ())), ())), ()),
                &[]);
  }

  #[test]
  fn bool_expr() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("1<2<=3 == false",
                &Prim2(Eq,
                       Box::new(Prim2(LessEq,
                                      Box::new(Prim2(Less,
                                                     Box::new(Number(1, ())),
                                                     Box::new(Number(2, ())), ())),
                                      Box::new(Number(3, ())), ())),
                       Box::new(Bool(false, ())), ()),
                &[]);
  }

  #[test]
  fn bool_expr2() {
    use self::Expr::*;
    use self::Prim2::*;

    test_parser("true || false",
                &Prim2(Or,
                       Box::new(Bool(true, ())),
                       Box::new(Bool(false, ())), ()),
                &[]);
  }
}
