fn main() {
  let mut a = 0;
  let b : *mut u8 = &mut a;
  let c : *mut u8 = &mut a;

  unsafe {
    println!("{} {}", *b, *c);
  }

  a = 42;

  unsafe {
    println!("{} {}", *b, *c);
  }
}

// Can have multiple mutable references with *mut pointers, but have to use
// unsafe blocks when dereferencing
