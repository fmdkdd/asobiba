use std::collections::BTreeMap;
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
  Comma,
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
    let pos = self.input.pos_of_next_char;
    match self.input.next() {
      None                        => panic!("{}:{}: Expected '{}' but found end of input instead",
                                            pos.line, pos.column, c),
      Some(actual) if actual != c => panic!("{}:{}: Expected '{}' but found '{}' instead",
                                            pos.line, pos.column, c, actual),
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
        None       => return s,
        Some(&'"') => break,
        Some(_)    => {},
      }
      // Have to put that there because self.input is borrowed in the match
      // above
      let next = self.input.next().unwrap();
      let pos = self.input.pos_of_next_char;

      // Backslash escapes the next character
      if next == '\\' {
        match self.input.peek() {
          None        => panic!("{}:{}: Unterminated string literal",
                                pos.line, pos.column),
          Some(&'"')      => s.push('"'),
          Some(&'n')      => s.push('\n'),
          Some(&'t')      => s.push('\t'),
          Some(&'r')      => s.push('\r'),
          Some(&'\\')     => s.push('\\'),
          Some(&'(')      => s.push('('),
          Some(&')')      => s.push(')'),
          Some(&'\u{0a}') => (), // newline
          Some(c)     => panic!("{}:{}: Invalid escape character: '{}'",
                                pos.line, pos.column, c),
        }
        // Discard the character after the backslash
        self.input.next();
      } else {
        s.push(next);
      }
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
    let pos = self.input.pos_of_next_char;
    s.parse::<i64>().expect(&format!("{}:{}: Failed to parse decimal number: '{}'",
                                     pos.line, pos.column, s))
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
      ','                 => { self.input.next(); self.emit(TokenKind::Comma) },
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

#[derive(PartialEq, Debug, Clone)]
pub struct ParseTree {
  pub roots: Vec<Node>,
  pub symbol_table: Vec<String>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
  pub kind: NodeKind,
  pub start: TextPosition,
  pub end: TextPosition,
}

#[derive(PartialEq, Debug, Clone)]
pub enum NodeKind {
  Sexp(Vec<Node>),
  Symbol(usize),
  String(String),
  Number(i64),
  Comment(String),
  Quote(Box<Node>),
  Backquote(Box<Node>),
  Pound(Box<Node>),
  Comma(Box<Node>),
}

pub struct Parser<'a> {
  input: TokenStream<'a>,
  symbol_table: BTreeMap<String, usize>,
}

impl<'a> Parser<'a> {
  pub fn new(input: &'a str) -> Self {
    Parser {
      input: TokenStream::new(input),
      symbol_table: BTreeMap::new(),
    }
  }

  pub fn parse(&mut self) -> ParseTree {
    let mut nodes = Vec::new();

    while !self.input.is_eof() {
      nodes.push(self.parse_node());
    }

    self.expect(&TokenKind::EOF);

    // Transform the Map<String, usize> into a Vec<String> where the strings are
    // put in increasing order of the map value.  The Vec<String> is actually a
    // Map<usize, String>, but since the index is exactly in [0, len], we can
    // use a Vec to represent the mapping.
    //
    // We have to extract the pairs into a temporary vector first.
    let mut pair_vector = Vec::with_capacity(self.symbol_table.len());
    for (symbol, index) in self.symbol_table.iter() {
      pair_vector.push((index, symbol.clone()));
    }
    // Sort the pairs so we have [(0, _), (1, _), ...]
    pair_vector.sort();

    // Now we just need to extract the symbols in order
    let mut symbol_vector = Vec::with_capacity(pair_vector.len());
    while !pair_vector.is_empty() {
      let (_, symbol) = pair_vector.remove(0);
      symbol_vector.push(symbol);
    }

    ParseTree {
      roots: nodes,
      symbol_table: symbol_vector,
    }
  }

  fn lookup_or_insert_symbol(&mut self, name: String) -> usize {
    if !self.symbol_table.contains_key(&name) {
      let index = self.symbol_table.len();
      self.symbol_table.insert(name, index);
      index
    } else {
      *self.symbol_table.get(&name).unwrap()
    }
  }

  fn expect(&mut self, kind: &TokenKind) -> Token {
    let t = self.input.next();
    if t.kind != *kind  {
      panic!("{}:{}-{}:{}: Expected token '{:?}', but got '{:?}'",
             t.start.line, t.start.column,
             t.end.line, t.end.column,
             kind, t.kind);
    }
    t
  }

  fn parse_number(&mut self) -> Node {
    let token = self.input.next();
    if let TokenKind::Number(n) = token.kind {
      Node {
        kind: NodeKind::Number(n),
        start: token.start,
        end: token.end,
      }
    } else {
      panic!("{}:{}-{}:{}: Expected number, got '{:?}'",
             token.start.line, token.start.column,
             token.end.line, token.end.column,
             token.kind);
    }
  }

  fn parse_string(&mut self) -> Node {
    let token = self.input.next();
    if let TokenKind::String(s) = token.kind {
      Node {
        kind: NodeKind::String(s),
        start: token.start,
        end: token.end,
      }
    } else {
      panic!("{}:{}-{}:{}: Expected string, got '{:?}'",
             token.start.line, token.start.column,
             token.end.line, token.end.column,
             token.kind);
    }
  }

  fn parse_symbol(&mut self) -> Node {
    let token = self.input.next();
    if let TokenKind::Ident(name) = token.kind {
      Node {
        kind: NodeKind::Symbol(self.lookup_or_insert_symbol(name)),
        start: token.start,
        end: token.end,
      }
    } else {
      panic!("{}:{}-{}:{}: Expected string, got '{:?}'",
             token.start.line, token.start.column,
             token.end.line, token.end.column,
             token.kind);
    }
  }

  fn parse_sexp(&mut self) -> Node {
    let start = self.expect(&TokenKind::LeftParen).start;

    let mut nodes = Vec::new();

    while self.input.peek().kind != TokenKind::RightParen {
      nodes.push(self.parse_node());
    }

    let end = self.expect(&TokenKind::RightParen).end;

    Node {
      kind: NodeKind::Sexp(nodes),
      start,
      end,
    }
  }

  fn parse_comment(&mut self) -> Node {
    let first_comment = self.input.next();

    if let TokenKind::Comment(text) = first_comment.kind {
      let start = first_comment.start;
      let mut end = first_comment.end;
      let mut comment_text = text;

      // Concatenate all successive comments, in order to have only one node in
      // the tree
      while let TokenKind::Comment(_) = self.input.peek().kind {
        let comment = self.input.next();
        if let TokenKind::Comment(c) = comment.kind {
          comment_text += &c;
          end = comment.end;
        }
      }

      Node {
        kind: NodeKind::Comment(comment_text),
        start: start,
        end: end,
      }
    } else {
      panic!("{}:{}-{}:{}: Expected comment, got '{:?}'",
             first_comment.start.line, first_comment.start.column,
             first_comment.end.line, first_comment.end.column,
             first_comment.kind);
    }
  }

  // For comma, backquote, etc
  fn parse_operator(&mut self, kind: &TokenKind) -> Node {
    let token = self.expect(kind);

    // Operators apply to the next node
    let node = self.parse_node();

    Node {
      start: token.start,
      end: node.end, // kind last because it moves `node` into the box
      kind: match kind {
        &TokenKind::Backquote => NodeKind::Backquote(Box::new(node)),
        &TokenKind::Quote     => NodeKind::Quote(Box::new(node)),
        &TokenKind::Comma     => NodeKind::Comma(Box::new(node)),
        &TokenKind::Pound     => NodeKind::Pound(Box::new(node)),
        _                     => panic!("Invalid token kind for parse_operator: '{:?}'", kind),
      },
    }
  }

  fn parse_node(&mut self) -> Node {
    use self::TokenKind::*;

    let token = self.input.peek().clone();
    match token.kind {
      LeftParen  => self.parse_sexp(),
      Backquote  => self.parse_operator(&Backquote),
      Quote      => self.parse_operator(&Quote),
      Comma      => self.parse_operator(&Comma),
      Pound      => self.parse_operator(&Pound),
      Comment(_) => self.parse_comment(),
      String(_)  => self.parse_string(),
      Ident(_)   => self.parse_symbol(),
      Number(_)  => self.parse_number(),
      _          => panic!("{}:{}-{}:{}: Unexpected token: '{:?}'",
                           token.start.line, token.start.column,
                           token.end.line, token.end.column, token.kind),
    }
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Tests
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#[cfg(test)]
mod tests {
  use super::{Node, NodeKind, Parser, TextPosition, Token, TokenKind, TokenStream};

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

    {
      let mut t = TokenStream::new(r#" "a\
b" "#);
      assert_eq!(TokenKind::String("ab".to_string()), t.next().kind);
    }
  }

  }

  #[test]
  fn character_position() {
    let mut t = TokenStream::new(r#"abc
def"#);
    assert_eq!(Token {
      kind  : TokenKind::Ident("abc".to_string()),
      start : TextPosition { pos: 0, line: 1, column: 0 },
      end   : TextPosition { pos: 2, line: 1, column: 2 },
    }, t.next());

    assert_eq!(Token {
      kind  : TokenKind::Ident("def".to_string()),
      start : TextPosition { pos: 4, line: 2, column: 0 },
      end   : TextPosition { pos: 6, line: 2, column: 2 },
    }, t.next());
  }

  #[test]
  fn parse_atom() {
    let mut p = Parser::new("atom");
    let t = p.parse();
    assert_eq!(t.roots, vec![
      Node {
        kind  : NodeKind::Symbol(0),
        start : TextPosition { pos: 0, line: 1, column: 0 },
        end   : TextPosition { pos: 3, line: 1, column: 3 },
      },
    ]);
  }

  #[test]
  fn parse_sexp() {
    let mut p = Parser::new(r#"(foo 23)"#);
    let t = p.parse();
    assert_eq!(vec!["foo"], t.symbol_table);
    assert_eq!(vec![
      Node {
        kind: NodeKind::Sexp(vec![
          Node {
            kind  : NodeKind::Symbol(0),
            start : TextPosition { pos: 1, line: 1, column: 1 },
            end   : TextPosition { pos: 3, line: 1, column: 3 },
          },
          Node {
            kind  : NodeKind::Number(23),
            start : TextPosition { pos: 5, line: 1, column: 5 },
            end   : TextPosition { pos: 6, line: 1, column: 6 },
          },
        ]),
        start : TextPosition { pos: 0, line: 1, column: 0 },
        end   : TextPosition { pos: 7, line: 1, column: 7 },
      },
    ], t.roots);
  }

  #[test]
  fn parse_multi_symbols() {
    let mut p = Parser::new(r#"(foo a foo a)"#);
    let t = p.parse();

    assert_eq!(vec!["foo", "a"], t.symbol_table);

    // Check symbols first
    if let NodeKind::Sexp(ref nodes) = t.roots[0].kind {
      assert_eq!(NodeKind::Symbol(0), nodes[0].kind);
      assert_eq!(NodeKind::Symbol(1), nodes[1].kind);
      assert_eq!(NodeKind::Symbol(0), nodes[2].kind);
      assert_eq!(NodeKind::Symbol(1), nodes[3].kind);
    }

    // Then the full shebang with positions
    assert_eq!(vec![
      Node {
        kind: NodeKind::Sexp(vec![
          Node {
            kind  : NodeKind::Symbol(0),
            start : TextPosition { pos: 1, line: 1, column: 1 },
            end   : TextPosition { pos: 3, line: 1, column: 3 },
          },
          Node {
            kind  : NodeKind::Symbol(1),
            start : TextPosition { pos: 5, line: 1, column: 5 },
            end   : TextPosition { pos: 5, line: 1, column: 5 },
          },
          Node {
            kind  : NodeKind::Symbol(0),
            start : TextPosition { pos: 7, line: 1, column: 7 },
            end   : TextPosition { pos: 9, line: 1, column: 9 },
          },
          Node {
            kind  : NodeKind::Symbol(1),
            start : TextPosition { pos: 11, line: 1, column: 11 },
            end   : TextPosition { pos: 11, line: 1, column: 11 },
          },
        ]),
        start : TextPosition { pos: 0,  line: 1, column: 0  },
        end   : TextPosition { pos: 12, line: 1, column: 12 },
      },
    ], t.roots);
  }

  #[test]
  fn parse_multiline() {
    let mut p = Parser::new(r#"
(foo a "bla" 23)
atom
"#);
    let t = p.parse();
    assert_eq!(vec!["foo", "a", "atom"], t.symbol_table);
    assert_eq!(vec![
      Node {
        kind: NodeKind::Sexp(vec![
          Node {
            kind  : NodeKind::Symbol(0),
            start : TextPosition { pos: 2, line: 2, column: 1 },
            end   : TextPosition { pos: 4, line: 2, column: 3 },
          },
          Node {
            kind  : NodeKind::Symbol(1),
            start : TextPosition { pos: 6, line: 2, column: 5 },
            end   : TextPosition { pos: 6, line: 2, column: 5 },
          },
          Node {
            kind  : NodeKind::String("bla".to_string()),
            start : TextPosition { pos: 8,  line: 2, column: 7  },
            end   : TextPosition { pos: 12, line: 2, column: 11 },
          },
          Node {
            kind  : NodeKind::Number(23),
            start : TextPosition { pos: 14, line: 2, column: 13 },
            end   : TextPosition { pos: 15, line: 2, column: 14 },
          },
        ]),
        start : TextPosition { pos: 1,  line: 2, column: 0 },
        end   : TextPosition { pos: 16, line: 2, column: 15 },
      },
      Node {
        kind  : NodeKind::Symbol(2),
        start : TextPosition { pos: 18, line: 3, column: 0 },
        end   : TextPosition { pos: 21, line: 3, column: 3 },
      },
    ], t.roots);
  }

  #[test]
  #[should_panic]
  fn parse_error() {
    let mut p = Parser::new("(unbalanced (paren)");
    p.parse();
  }

  #[test]
  fn parse_quote() {
    let mut p = Parser::new("(foo `(1 ,a))");
    let t = p.parse();

    assert_eq!(vec!["foo", "a"], t.symbol_table);

    println!("{:?}", t.roots[0].kind);

    if let NodeKind::Sexp(ref nodes) = t.roots[0].kind {
      assert_eq!(NodeKind::Symbol(0), nodes[0].kind);
      if let NodeKind::Backquote(ref quoted) = nodes[1].kind {
        if let NodeKind::Sexp(ref quoted_sexp) = quoted.kind {
          assert_eq!(NodeKind::Number(1), quoted_sexp[0].kind);
          if let NodeKind::Comma(ref unquoted) = quoted_sexp[1].kind {
            assert_eq!(NodeKind::Symbol(1), unquoted.kind);
            return;
          }
        }
      }
    }
    assert!(false);
  }
}
