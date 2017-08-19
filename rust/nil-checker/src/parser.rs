use std::iter::Peekable;
use std::str::Chars;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tokenizer
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  EOF,
  LeftParen,
  RightParen,
  Quote,
  Backquote,
  Pound,
  Comment(String),
  String(String),
  Ident(String),
  Number(i64),
}

pub struct TokenStream<'a> {
  input: Peekable<Chars<'a>>,
  peeked_token: Option<Token>,
}

impl<'a> TokenStream<'a> {
  pub fn new(input: &'a str) -> Self {
    TokenStream {
      input: input.chars().peekable(),
      peeked_token: None,
    }
  }

  fn expect(&mut self, c: char) {
    match self.input.next() {
      None                        => panic!("Expected '{}' but found end of input instead"),
      Some(actual) if actual != c => panic!("Expected '{}' but found '{}' instead", c, actual),
      _                           => {},
    }
  }

  fn eat_space(&mut self) {
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

  fn eat_delimiter(&mut self) {
    loop {
      match self.input.peek() {
        None                 => return,
        Some(c) if *c != ';' => return,
        Some(_)              => {},
      }
      // Do it outside because of borrowing
      self.input.next();
    }
  }

  fn read_string(&mut self) -> String {
    // Discard first quote
    self.expect('"');

    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => return s,
        Some(&'"')                   => break,
        Some(_)                      => {},
      }
      // Have to put that there because self.input is borrowed in the match
      // above
      let mut next = self.input.next().unwrap();

      // Backslash escapes the next character
      if next == '\\' {
        match self.input.peek() {
          None        => panic!("Unterminated string literal"),
          Some(&'"')  => next = '"',
          Some(&'n')  => next = '\n',
          Some(&'t')  => next = '\t',
          Some(&'\\') => next = '\\',
          Some(c)     => panic!("Invalid escape character: '{}'", c),
        }
        // Discard the character after the backslash
        self.input.next();
      }
      s.push(next);
    }
    // Discard end quote
    self.expect('"');
    s
  }

  fn read_comment(&mut self) -> String {
    // Discard starting comment delimiters
    self.eat_delimiter();

    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => break,
        Some(&'\n')                  => break,
        Some(_)                      => {},
      }
      // Have to put that there because self.input is borrowed in the match
      // above
      s.push(self.input.next().unwrap());
    }
    s
  }

  fn read_number(&mut self) -> i64 {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                       => break,
        Some(c) if !c.is_digit(10) => break,
        Some(_)                    => {},
      }
      // Have to put that there because self.input is borrowed in the match
      // above
      s.push(self.input.next().unwrap());
    }
    s.parse::<i64>().expect(&format!("Failed to parse decimal number: '{}'", s))
  }

  fn read_identifier(&mut self) -> String {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => break,
        Some(c) if c.is_whitespace() => break,
        Some(&'(') | Some(&')') | Some(&'"')
          | Some(&'\'') | Some(&';') => break,
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
    // Skip whitespace
    self.eat_space();

    // Bail if no more input
    if let None = self.input.peek() {
      return Token::EOF
    }

    // Have to dance around since peek borrows self.input
    let c = *self.input.peek().unwrap();
    match c {
      '('                 => { self.input.next(); Token::LeftParen },
      ')'                 => { self.input.next(); Token::RightParen },
      '\''                => { self.input.next(); Token::Quote },
      '#'                 => { self.input.next(); Token::Pound },
      '`'                 => { self.input.next(); Token::Backquote },
      '"'                 => Token::String(self.read_string()),
      ';'                 => Token::Comment(self.read_comment()),
      _ if c.is_digit(10) => Token::Number(self.read_number()),
      _                   => Token::Ident(self.read_identifier()),
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
    self.peek() == &Token::EOF
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Parser
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

enum Node {
  Sexp(Vec<Node>),
  Atom(Atom),
}

#[derive(Debug, Clone)]
enum Atom {
  Ident(String),
  String(String),
  Number(u32),
}

fn parse(input: &str) {

}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tests
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[cfg(test)]
mod tests {
  use super::{Token, TokenStream};

  #[test]
  fn sexp() {
    let expected = [Token::Comment(" Comment".to_string()),
                    Token::LeftParen,
                    Token::Ident("defun".to_string()),
                    Token::Ident("foo".to_string()),
                    Token::LeftParen,
                    Token::Ident("bla".to_string()),
                    Token::Ident("comp".to_string()),
                    Token::RightParen,
                    Token::Comment(" other comment".to_string()),
                    Token::String("multi-line\ndocstring".to_string()),
                    Token::LeftParen,
                    Token::Ident("progn".to_string()),
                    Token::Number(1),
                    Token::RightParen,
                    Token::RightParen,
                    Token::EOF];
    let mut t = TokenStream::new(r#"
;; Comment
(defun foo (bla comp) ; other comment
  "multi-line
docstring"
  (progn 1))"#);
    for token in expected.iter() {
      assert_eq!(*token, t.next());
    }
    assert!(t.is_eof());
  }

  #[test]
  fn peek() {
    let mut t = TokenStream::new("ba bi bo");
    assert_eq!(&Token::Ident("ba".to_string()), t.peek());
    assert_eq!(&Token::Ident("ba".to_string()), t.peek());
    assert_eq!( Token::Ident("ba".to_string()), t.next());
    assert_eq!(&Token::Ident("bi".to_string()), t.peek());
    assert_eq!(&Token::Ident("bi".to_string()), t.peek());
    assert_eq!( Token::Ident("bi".to_string()), t.next());
    assert_eq!( Token::Ident("bo".to_string()), t.next());
    assert_eq!(&Token::EOF, t.peek());
    assert_eq!( Token::EOF, t.next());
    assert_eq!( Token::EOF, t.next());
  }

  #[test]
  fn escaped_string() {
    {
      let mut t = TokenStream::new(r#"  "b\"i" "#);
      assert_eq!(Token::String("b\"i".to_string()), t.next());
    }

    {
      let mut t = TokenStream::new(r#" "a\nb" "#);
      assert_eq!(Token::String("a\nb".to_string()), t.next());
    }
  }
}
