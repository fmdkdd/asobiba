fn main() {
  let mut x = 42;

  {
    let y = &mut x;
    let z = &mut x;
    let zz = &mut x;
  }
}
