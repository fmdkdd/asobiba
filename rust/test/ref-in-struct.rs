// With references
fn smallest(v: &[i32]) -> &i32 {
  let mut s = &v[0];
  for r in &v[1..] {
    if r < s {
      s = r;
    }
  }
  s
}

// Can work with any T
fn smallest_t<T: PartialOrd>(v: &[T]) -> &T {
  let mut s = &v[0];
  for r in &v[1..] {
    if r < s {
      s = r;
    }
  }
  s
}

// But since i32 is Copy, we can also copy values out of the slice
fn smallest_copy(v: &[i32]) -> i32 {
  let mut s = v[0];
  for r in &v[1..] {
    if *r < s {
      s = *r;
    }
  }
  s
}


fn main() {
  // Here the scope of parabola has to exceed the scope of s.
  {
    // The order of declaration is important, since they are dropped in reverse
    // order.  If we reverse them, parabola (the referent) would be dropped
    // before s, which would become dangling.
    let parabola;
    let s;
    {
      parabola = [9,4,1,0];
      s = smallest(&parabola);
    }
    assert_eq!(*s, 0);
  }

  // The generic version works as well
  {
    let parabola;
    let s;
    {
      parabola = [9,4,1,0];
      s = smallest_t(&parabola);
    }
    assert_eq!(*s, 0);
  }

  // Here parabola can be dropped before s, since the smallest element has moved
  // to s.  The borrowing of parabola ends after the function call, not when s
  // is dropped.
  {
    let s;
    {
      let parabola = [9,4,1,0];
      s = smallest_copy(&parabola);
    }
    assert_eq!(s, 0);
  }
}
