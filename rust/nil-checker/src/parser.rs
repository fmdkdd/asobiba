use std::iter::Peekable;
use std::str::Chars;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Character stream
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// This is only to record position, line and column information when building
// the tokens

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct TextPosition {
  pos: u32,                     // absolute character position
  line: u32,                    // line number (starts at 1)
  column: u32,                  // column number (starts at 0)
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

  fn peek(&mut self) -> Option<&char> {
    self.input.peek()
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
  Quote,
  Backquote,
  Pound,
  Comment(String),
  String(String),
  Ident(String),
  Number(i64),
}

pub struct TokenStream<'a> {
  input: CharStream<'a>,
  peeked_token: Option<Token>,
  saved_position: Option<TextPosition>,
}

impl<'a> TokenStream<'a> {
  pub fn new(input: &'a str) -> Self {
    TokenStream {
      input: CharStream::new(input),
      peeked_token: None,
      saved_position: None,
    }
  }

  fn expect(&mut self, c: char) {
    match self.input.next() {
      None                        => panic!("Expected '{}' but found end of input instead"),
      Some(actual) if actual != c => panic!("Expected '{}' but found '{}' instead", c, actual),
      _                           => {},
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
      return self.emit(TokenKind::EOF)
    }

    // Have to dance around since peek borrows self.input
    let c = *self.input.peek().unwrap();
    match c {
      '('                 => { self.input.next(); self.emit(TokenKind::LeftParen) },
      ')'                 => { self.input.next(); self.emit(TokenKind::RightParen) },
      '\''                => { self.input.next(); self.emit(TokenKind::Quote) },
      '#'                 => { self.input.next(); self.emit(TokenKind::Pound) },
      '`'                 => { self.input.next(); self.emit(TokenKind::Backquote) },
      '"'                 => { self.save_position(); let t = TokenKind::String(self.read_string());    self.emit(t) },
      ';'                 => { self.save_position(); let t = TokenKind::Comment(self.read_comment());  self.emit(t) },
      _ if c.is_digit(10) => { self.save_position(); let t = TokenKind::Number(self.read_number());    self.emit(t) },
      _                   => { self.save_position(); let t = TokenKind::Ident(self.read_identifier()); self.emit(t) },
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
  Number(i64),
}

fn parse(input: &str) {

}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tests
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[cfg(test)]
mod tests {
  use super::{TextPosition, Token, TokenKind, TokenStream};

  #[test]
  fn sexp() {
    let expected = [TokenKind::Comment(" Comment".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Ident("defun".to_string()),
                    TokenKind::Ident("foo".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Ident("bla".to_string()),
                    TokenKind::Ident("comp".to_string()),
                    TokenKind::RightParen,
                    TokenKind::Comment(" other comment".to_string()),
                    TokenKind::String("multi-line\ndocstring".to_string()),
                    TokenKind::LeftParen,
                    TokenKind::Ident("progn".to_string()),
                    TokenKind::Number(1),
                    TokenKind::RightParen,
                    TokenKind::RightParen,
                    TokenKind::EOF];
    let mut t = TokenStream::new(r#"
;; Comment
(defun foo (bla comp) ; other comment
  "multi-line
docstring"
  (progn 1))"#);
    for token in expected.iter() {
      assert_eq!(*token, t.next().kind);
    }
    assert!(t.is_eof());
  }

  #[test]
  fn peek() {
    let mut t = TokenStream::new("ba bi bo");
    assert_eq!(TokenKind::Ident("ba".to_string()), t.peek().kind);
    assert_eq!(TokenKind::Ident("ba".to_string()), t.peek().kind);
    assert_eq!(TokenKind::Ident("ba".to_string()), t.next().kind);
    assert_eq!(TokenKind::Ident("bi".to_string()), t.peek().kind);
    assert_eq!(TokenKind::Ident("bi".to_string()), t.peek().kind);
    assert_eq!(TokenKind::Ident("bi".to_string()), t.next().kind);
    assert_eq!(TokenKind::Ident("bo".to_string()), t.next().kind);
    assert_eq!(TokenKind::EOF, t.peek().kind);
    assert_eq!(TokenKind::EOF, t.next().kind);
    assert_eq!(TokenKind::EOF, t.next().kind);
  }

  #[test]
  fn escaped_string() {
    {
      let mut t = TokenStream::new(r#"  "b\"i" "#);
      assert_eq!(TokenKind::String("b\"i".to_string()), t.next().kind);
    }

    {
      let mut t = TokenStream::new(r#" "a\nb" "#);
      assert_eq!(TokenKind::String("a\nb".to_string()), t.next().kind);
    }
  }

  #[test]
  fn character_position() {
    let mut t = TokenStream::new(r#"abc
def"#);
    assert_eq!(Token {
      kind: TokenKind::Ident("abc".to_string()),
      start: TextPosition { pos: 0, line: 1, column: 0 },
      end: TextPosition { pos: 2, line: 1, column: 2 },
    }, t.next());

    assert_eq!(Token {
      kind: TokenKind::Ident("def".to_string()),
      start: TextPosition { pos: 4, line: 2, column: 0 },
      end: TextPosition { pos: 6, line: 2, column: 2 },
    }, t.next());
  }
}
