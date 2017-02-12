struct S {}

fn main() {
  let x = S {};
  let y = x;
  let z = x;

  let unused = 2;
}
