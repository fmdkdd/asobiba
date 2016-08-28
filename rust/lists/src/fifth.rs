use std::mem;
use std::ptr;

pub struct List<T> {
  head: Link<T>,
  tail: *mut Node<T>,
}

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
  elem: T,
  next: Link<T>,
}

impl<T> List<T> {
  pub fn new() -> Self {
    List { head: None, tail: ptr::null_mut() }
  }

  pub fn push(&mut self, elem: T) {
    let mut new_tail = Box::new(Node {
      elem: elem,
      next: None,
    });

    let raw_tail: *mut _ = &mut *new_tail;

    if !self.tail.is_null() {
      unsafe {
        (*self.tail).next = Some(new_tail);
      }
    } else {
      self.head = Some(new_tail);
    }

    self.tail = raw_tail;
  }
}
