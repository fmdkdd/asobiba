use std::collections::HashMap;
use std::hash::Hash;

trait A : Eq + Hash {}
trait B : A {}
trait C : A {}

#[derive(PartialEq, Eq, Hash)]
struct AStruct {}

#[derive(PartialEq, Eq, Hash)]
struct BStruct {}
impl A for BStruct {}
impl B for BStruct {}

#[derive(PartialEq, Eq, Hash)]
struct CStruct {}
impl A for CStruct {}
impl C for CStruct {}

struct Map<'a, E: A + 'a> {
  map: HashMap<&'a E, &'a E>,
}

impl<'a, E: A> Map<'a, E> {

  fn put(&mut self, key: E, value: E) {
    self.map.insert(&key, &value);
  }
}

fn main() {
  let b = BStruct {};
  let c = CStruct {};

  let map : Map<&A> = Map { map: HashMap::new() };

  map.put(b, b);
  map.put(b, c);
  map.put(c, c);
}
