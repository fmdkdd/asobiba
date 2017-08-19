use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq, Debug, Clone)]
enum Token {
  EOF,
  LeftParen,
  RightParen,
  Atom(String),
}

struct TokenStream<'a> {
  input: Peekable<Chars<'a>>,
  peeked_token: Option<Token>,
}

impl<'a> TokenStream<'a> {
  fn new(input: &'a str) -> Self {
    TokenStream {
      input: input.chars().peekable(),
      peeked_token: None,
    }
  }

  fn read_space(&mut self) {
    loop {
      match self.input.peek() {
        None                          => return,
        Some(c) if !c.is_whitespace() => return,
        Some(_)                       => {},
      }
      // Do it outside because of borrowing
      self.input.next();
    }
  }

  fn read_atom(&mut self) -> String {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => break,
        Some(c) if c.is_whitespace() => break,
        Some(&'(') | Some(&')')      => break,
        Some(_)                      => {},
      }
      // Have to put that there because self.input is borrowed in the match
      // above
      s.push(self.input.next().unwrap());
    }
    s
  }

  // Consume input until we have eaten another token
  fn advance(&mut self) -> Token {
    // Bail if no more input
    if let None = self.input.peek() {
      return Token::EOF
    }

    // Skip whitespace
    self.read_space();

    // Have to dance around since peek borrows self.input
    let c = *self.input.peek().unwrap();
    match c {
      '(' => { self.input.next(); Token::LeftParen },
      ')' => { self.input.next(); Token::RightParen },
      _   => Token::Atom(self.read_atom()),
    }
  }

  fn peek(&mut self) -> &Token {
    if self.peeked_token.is_none() {
      self.peeked_token = Some(self.advance());
    }
    &self.peeked_token.as_ref().unwrap()
  }

  fn next(&mut self) -> Token {
    if self.peeked_token.is_some() {
      let ret = self.peeked_token.clone().unwrap();
      self.peeked_token = None;
      ret
    } else {
      self.advance()
    }
  }

  fn is_eof(&mut self) -> bool {
    self.peek() == &Token::EOF
  }
}

#[cfg(test)]
mod tests {
  use super::{Token, TokenStream};

  #[test]
  fn sexp() {
    let expected = [Token::LeftParen,
                    Token::Atom("defun".to_string()),
                    Token::Atom("foo".to_string()),
                    Token::LeftParen,
                    Token::Atom("bla".to_string()),
                    Token::Atom("comp".to_string()),
                    Token::RightParen,
                    Token::LeftParen,
                    Token::Atom("progn".to_string()),
                    Token::Atom("1".to_string()),
                    Token::RightParen,
                    Token::RightParen,
                    Token::EOF];
    let mut t = TokenStream::new("(defun foo (bla comp) (progn 1))");
    for token in expected.iter() {
      assert_eq!(*token, t.next());
    }
    assert!(t.is_eof());
  }

  #[test]
  fn peek() {
    let mut t = TokenStream::new("ba bi bo");
    assert_eq!(&Token::Atom("ba".to_string()), t.peek());
    assert_eq!(&Token::Atom("ba".to_string()), t.peek());
    assert_eq!( Token::Atom("ba".to_string()), t.next());
    assert_eq!(&Token::Atom("bi".to_string()), t.peek());
    assert_eq!(&Token::Atom("bi".to_string()), t.peek());
    assert_eq!( Token::Atom("bi".to_string()), t.next());
    assert_eq!( Token::Atom("bo".to_string()), t.next());
    assert_eq!(&Token::EOF, t.peek());
    assert_eq!( Token::EOF, t.next());
    assert_eq!( Token::EOF, t.next());
  }

}
