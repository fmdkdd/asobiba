use std::rc::Rc;

#[derive(Debug, Copy, Clone)]
enum AtomType { Terminal, NonTerminal }

#[derive(Debug)]
enum Ptr {
  Conc { left: Rc<Ptr>, right: Rc<Ptr> },
  Atom { code: char, action: usize, a_type: AtomType },
}

trait Print {
  fn print(&self, n: usize) -> String;
}

impl Print for Ptr {
  fn print(&self, n: usize) -> String {
    match self {
      &Ptr::Conc { ref left, ref right} =>
        format!("{}> Conc\n{}\n{}\n",
                indent(n), left.print(n + 3), right.print(n + 3)),

      &Ptr::Atom {code, action, a_type} =>
        format!("{}> Atom {} {} {:?}",
                indent(n), code, action, a_type)
    }
  }
}

fn indent(n: usize) -> String {
  let mut s = String::with_capacity(n);
  for _ in 0..n {
    s.push('-');
  }
  s
}

fn main() {
  let a = Rc::new(Ptr::Atom { code: ';', action: 0, a_type: AtomType::Terminal });
  let p = Ptr::Conc { left: a.clone(), right: a.clone() };

  println!("{:?}", p);
  println!("{}", p.print(0));
}
