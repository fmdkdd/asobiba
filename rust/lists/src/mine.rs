use std::mem;

struct Link {
  elem: i32,
  next: Option<Box<Link>>,
}

pub struct List {
  head: Option<Box<Link>>,
}

impl List {
  pub fn new() -> Self {
    List {
      head: None
    }
  }

  pub fn push(&mut self, elem: i32) {
    let new_head = Some(Box::new(Link {
      elem: elem,
      next: mem::replace(&mut self.head, None),
    }));

    self.head = new_head;
  }

  pub fn pop(&mut self) -> Option<i32> {
    match mem::replace(&mut self.head, None) {
      None => None,

      Some(boxed_link) => {
        let link = *boxed_link;
        self.head = link.next;
        Some(link.elem)
      }
    }
  }

  pub fn peek(&self) -> Option<i32> {
    match self.head {
      None => None,
      Some(ref link) => Some(link.elem),
    }
  }
}

impl Drop for List {
  fn drop(&mut self) {
    let mut cur_link = mem::replace(&mut self.head, None);

    while let Some(mut boxed_link) = cur_link {
      cur_link = mem::replace(&mut boxed_link.next, None);
    }
  }
}


#[cfg(test)]
mod tests {
  use super::List;

  #[test]
  fn basics() {
    let mut list = List::new();

    // Check empty list behaves right
    assert_eq!(list.pop(), None);

    // Populate list
    list.push(1);
    list.push(2);
    list.push(3);

    // Check normal removal
    assert_eq!(list.pop(), Some(3));
    assert_eq!(list.pop(), Some(2));

    // Push some more just to make sure nothing's corrupted
    list.push(4);
    list.push(5);

    // Check normal removal
    assert_eq!(list.pop(), Some(5));
    assert_eq!(list.pop(), Some(4));

    // Check exhaustion
    assert_eq!(list.pop(), Some(1));
    assert_eq!(list.pop(), None);
    assert_eq!(list.peek(), None);
  }

  #[test]
  fn drop() {
    let mut list = List::new();

    for i in 1..100000 {
      list.push(i);
    }

    // This will stack overflow when dropping the list with the default Drop
    // implementation
  }
}
