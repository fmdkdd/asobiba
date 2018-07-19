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
  pub kind: TokenKind,
  pub start: TextPosition,
  pub end: TextPosition,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
  EOF,
  LeftParen,
  RightParen,
  Colon,
  Comma,
  Equals,
  Bang,
  BinOp(BinOp),
  Keyword(Keyword),
  Ident(usize),
  Number(i32),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
  Let,
  In,
  If,
  Else,
  Add1,
  Sub1,
  Print,
  IsBool,
  IsNum,
  True,
  False,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinOp {
  Plus,
  Minus,
  Mult,
  Greater,
  GreaterEq,
  Less,
  LessEq,
  Eq,
  And,
  Or,
}

pub struct TokenStream<'a> {
  input: CharStream<'a>,
  peeked_token: Option<Token>,
  saved_position: Option<TextPosition>,
  pub symbols: Vec<String>,
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

  fn read_number(&mut self) -> i32 {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                       => break,
        Some(c) if !c.is_digit(10) => break,
        Some(_)                    => s.push(self.input.next().unwrap()),
      }
    }
    let pos = self.input.pos_of_next_char;
    s.parse::<i32>().expect(&format!("{}:{}: Failed to parse decimal number: '{}'",
                                     pos.line, pos.column, s))
  }

  fn read_word(&mut self) -> TokenKind {
    let mut s = String::new();
    loop {
      match self.input.peek() {
        None                         => break,
        Some(c) if c.is_whitespace() => break,
        Some('(') | Some(')') | Some(',') |
        Some(':') | Some('=') | Some('+') |
        Some('-') | Some('*') | Some('!') => break,
        Some(_)                      => s.push(self.input.next().unwrap()),
      }
    }

    // It's either a keyword or a plain identifier
    match s.as_str() {
      "let"    => TokenKind::Keyword(Keyword::Let),
      "in"     => TokenKind::Keyword(Keyword::In),
      "if"     => TokenKind::Keyword(Keyword::If),
      "else"   => TokenKind::Keyword(Keyword::Else),
      "add1"   => TokenKind::Keyword(Keyword::Add1),
      "sub1"   => TokenKind::Keyword(Keyword::Sub1),
      "print"  => TokenKind::Keyword(Keyword::Print),
      "isbool" => TokenKind::Keyword(Keyword::IsBool),
      "isnum"  => TokenKind::Keyword(Keyword::IsNum),
      "true"   => TokenKind::Keyword(Keyword::True),
      "false"  => TokenKind::Keyword(Keyword::False),

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
      '!'                 => { self.input.next(); self.emit(TokenKind::Bang) },
      '+'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Plus)) },
      '-'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Minus)) },
      '*'                 => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Mult)) },
      '='                 => {
        self.input.next();
        match self.input.peek() {
          Some('=') => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Eq)) },
          Some(_)   => self.emit(TokenKind::Equals),
          None      => self.emit(TokenKind::EOF)
        }
      },

      '&'                 => {
        self.input.next();
        match self.input.peek() {
          Some('&') => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::And)) },
          t         => panic!("Expected '&', got '{:?}'", t)
        }
      }

      '|'                 => {
        self.input.next();
        match self.input.peek() {
          Some('|') => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::Or)) },
          t         => panic!("Expected '|', got '{:?}'", t)
        }
      }

      '<'                 => {
        self.input.next();
        match self.input.peek() {
          Some('=') => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::LessEq)) },
          Some(_)   => self.emit(TokenKind::BinOp(BinOp::Less)),
          None      => self.emit(TokenKind::EOF)
        }
      }

      '>'                 => {
        self.input.next();
        match self.input.peek() {
          Some('=') => { self.input.next(); self.emit(TokenKind::BinOp(BinOp::GreaterEq)) },
          Some(_)   => self.emit(TokenKind::BinOp(BinOp::Greater)),
          None      => self.emit(TokenKind::EOF)
        }
      }

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
    use self::TokenKind::*;
    use self::Keyword::*;

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
    use self::TokenKind::*;
    use self::BinOp::*;

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
    use self::TokenKind::*;
    use self::Keyword::*;
    use self::BinOp::*;

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
    use self::TokenKind::*;

    let tokens = [
      Number(2),
      Number(3),
    ];
    test_lexer("# ignored
2
# ignored
3 # + 2", &tokens, &vec![]);
  }

  #[test]
  fn bool_ops() {
    use self::TokenKind::*;
    use self::BinOp::*;

    let tokens = [
      Number(1),
      BinOp(Less),
      Number(2),
      BinOp(LessEq),
      Number(3),
      BinOp(Eq),
      Number(4),
      BinOp(GreaterEq),
      Number(5),
      BinOp(Greater),
      Number(6),
    ];
    test_lexer("1<2<=3==4>=5>6", &tokens, &vec![]);;
  }

  #[test]
  fn boolexpr() {
    use self::TokenKind::*;
    use self::BinOp::*;
    use self::Keyword::*;

    let tokens = [
      Keyword(True),
      BinOp(Eq),
      Bang,
      Keyword(False),
    ];
    test_lexer("true==!false", &tokens, &vec![]);;
  }

  #[test]
  fn boolexpr2() {
    use self::TokenKind::*;
    use self::BinOp::*;
    use self::Keyword::*;

    let tokens = [
      Keyword(True),
      BinOp(Or),
      Keyword(False),
    ];
    test_lexer("true || false", &tokens, &vec![]);;
  }
}
