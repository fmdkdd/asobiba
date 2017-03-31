// Predictive parser from Dragon book p.227

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol {
  NonTerminal(String),
  Terminal(TerminalType),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum TerminalType {
  Ident,
  Arrow,
  Term,
  EOF,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Token {
  code: TerminalType,
  lexeme: Option<String>,
}

#[derive(Debug)]
struct Grammar {
  rules: Vec<Rule>,
}

#[derive(Debug, Clone)]
struct Rule {
  head: Symbol,
  body: Vec<Symbol>,
}

struct ParsingTable {
  table: HashMap<(Symbol, TerminalType), Rule>,
}

impl ParsingTable {
  fn get(&self, x: &Symbol, a: &TerminalType) -> Option<&Rule> {
    println!("M[{:?},{:?}]", x, a);
    self.table.get(&((*x).clone(), (*a).clone()))
  }

  fn put(&mut self, x: Symbol, a: TerminalType, r: Rule) {
    self.table.insert((x, a), r);
  }
}

fn lex<'a>(input: &String, idx: &mut usize) -> Option<Token> {
  let symbols = vec![Token { code: TerminalType::Ident,
                             lexeme: Some("S".to_string()) },
                     Token { code: TerminalType::Arrow,
                             lexeme: None },
                     Token { code: TerminalType::Term,
                             lexeme: Some("a".to_string()) },
                     Token { code: TerminalType::EOF,
                             lexeme: None }];

  let out = if *idx < symbols.len() { Some(symbols[*idx].clone()) } else { None };
  *idx += 1;
  out
}

fn matches(x: &Symbol, a: &Token) -> bool {
  if let &Symbol::Terminal(t_code) = x {
    t_code == a.code
  }
  else {
    false
  }
}

fn parse(table: &ParsingTable, start: Symbol, input: &String) {
  let mut stack = vec![start];
  let mut idx = 0;
  let mut a = lex(&input, &mut idx).unwrap();
  let mut rules = Vec::new();

  while stack.len() > 0 {

    println!("stack: {:?}", stack);

    let x = stack.pop().unwrap();
    if matches(&x, &a)  {
      a = lex(input, &mut idx).unwrap();
    }
    else if let Symbol::Terminal(_) = x {
      panic!("Syntax error: unexpected terminal");
    }
    else if let Some(r) = table.get(&x, &a.code) {
      rules.push(r);
      for s in r.body.iter().rev() {
        stack.push(s.clone());
      }
    }
    else {
      panic!("Syntax error: not in parsing table");
    }
  }
}

fn N(s: &str) -> Symbol {
  Symbol::NonTerminal(s.to_string())
}

fn T(s: TerminalType) -> Symbol {
  Symbol::Terminal(s)
}

fn main() {
  let mut table = ParsingTable { table: HashMap::new() };
  table.put(N("Rule"), TerminalType::Ident,
            Rule { head: N("Rule"),
                   body: vec![N("Head"),
                              T(TerminalType::Arrow),
                              N("Body")] });
  table.put(N("Body"), TerminalType::Term,
              Rule { head: N("Body"),
                     body: vec![T(TerminalType::Term)] });
  table.put(N("Head"), TerminalType::Ident,
            Rule { head: N("Head"),
                   body: vec![T(TerminalType::Ident)] });

  let mut G = parse(&table, N("G"), &String::new());
  println!("Parsed grammar: {:?}", G);
}
