#[derive(Debug)]
struct SNotCopy {
  x: u8,
}

fn f(s: SNotCopy) -> SNotCopy {
  // f takes ownership of s, but can't seem to be able to mutate it without
  // passing by reference
  // s.x += 1;
  println!("in f {:?}", s);
  s
}

fn f2(s: &mut SNotCopy) {
  s.x += 1;
  println!("in f2 {:?}", s);
  // Cannot move out of borrowed content
  // *s
}

fn main() {
  {
    // 'a' does not need to be mutable, even though we will eventually mutate its
    // content after we move them to 'b'.  That means that 'mut' indicates whether
    // the /variable/ is used in mutable contexts, not whether its contents can
    // actually mutate.
    let a = SNotCopy { x: 0 };
    // 'a' moves into 'b', so we cannot access 'a' anymore
    let b = a;
    println!("after init {:?}", b);
    let mut c = f(b);
    // Can't use 'b' after that
    println!("after f {:?}", c);
    let d = f2(&mut c);
    println!("after f2 {:?}", c);
    // a = b = c, but only one variable owns the value
  }

  {
    // Same remark for 'a', but here its contents are never mutated since it's
    // all copies
    let a = SCopy { x: 0 };
    // 'b' is a copy of 'a', so both are accessible and distinct
    let mut b = a;
    b.x += 1;
    println!("a: {:?}, b: {:?}", a, b);

    // 'c' should be another copy
    let mut c = f3(b);
    c.x += 1;
    println!("a: {:?}, b: {:?}, c: {:?}", a, b, c);

    let mut d = f4(&mut c);
    d.x += 1;
    println!("a: {:?}, b: {:?}, c: {:?}, d: {:?}", a, b, c, d);
    // a != b != c != d
  }

}


#[derive(Debug, Copy, Clone)]
struct SCopy {
  x: u8,
}

fn f3(s: SCopy) -> SCopy {
  // Still can't mutate if not a mutable reference, even though 's' is now a
  // copy that we own for the duration of this function
  // s.x += 1;
  println!("in f {:?}", s);
  // and a copy of 's' is returned
  s
}


fn f4(s: &mut SCopy) -> SCopy {
  s.x += 1;
  println!("in f4 {:?}", s);
  // are we returning a copy here?
  // (the caller already has 's', so returning a reference would be useless)
  *s
}
