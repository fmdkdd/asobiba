// From https://stackoverflow.com/questions/38078936/borrowing-reference-in-structure/38080934

struct Notificator {
  data: Vec<u64>,
}

impl Notificator {
  pub fn new(v: Vec<u64>) -> Notificator {
    Notificator {data: v}
  }

  pub fn push(&mut self, v: Vec<u64>) {
    self.data.extend(v)
  }

  pub fn resolving_events(&mut self) {
    println!("{:p}", self);
    let (slf, events) = self.check_for_events();
    println!("{:p}", slf);
    slf.events_execution(events)
  }

  // Pass immutable reference to self in return, to workaround the borrow
  // checker restricting the call self.events_execution above in the lifetime of
  // events.
  // Should still be safe according to:
  // https://internals.rust-lang.org/t/relaxing-the-borrow-checker-for-fn-mut-self-t/3256/16
  fn check_for_events(&mut self) -> (&Self, &Vec<u64>) {
    (self, &self.data)
  }

  fn events_execution(&self, events: &Vec<u64>) {
    for event in events.iter() {
      println!("{}", event);
    }
  }
}

fn main() {
  let mut n = Notificator::new(vec![1,2,3]);
  n.resolving_events();
  println!("Pushing...");
  n.push(vec![1,2,5]);
  n.resolving_events();
  println!("Pushing...");
  n.push(vec![1, 1, 1]);
  n.resolving_events();
}
