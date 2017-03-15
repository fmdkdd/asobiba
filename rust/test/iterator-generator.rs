// Trying to see if we can modify an iterator after creation.  That seems
// unsafe, so it probably won't work.

fn main() {

  let mut f = 1;

  let it = (0..).map(|n| n + f);

  let a : Vec<u32> = it.take(1).collect();
  println!("{}", a[0]);

  f = 2;

  let b : Vec<u32> = it.take(1).collect();
  println!("{}", b[0]);
}

// Indeed, f is borrowed by the closure, so you can't modify it while `it` is
// around.
