// By imposing an Into trait bound on the argument, the function can be used for
// several types.
fn ffffffffffffffff<T: Into<u32>>(x: T) {
  let y : u32 = x.into();
  println!("{}", y);
}

fn main () {
  ffffffffffffffff(10u16);
  ffffffffffffffff(10u8);
}

// Looking with `nm` in the compiled binary, there are three entries for
// `ffff..`, but only two if we remove one of the calls in main.  So there is
// specialization going on even in debug builds.
