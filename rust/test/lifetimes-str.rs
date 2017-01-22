fn main() {
  // 'a' does not live long enough, as expected because it will be dropped at
  // the end of its block, but 'c' still refers to its value.
  {
    let mut c = None;
    {
      let a = Vec::new();
      c = Some(id(&a));
    }
    println!("{:?}", c);
  }

  // But here, even though 'c' can refer to the value of 'a', there's no issue.
  // Mysterious.  Seems the string is /not/ dropped when it goes out of scope?
  // But it's still out of scope, so not through 'a', but still through 'c'.
  {
    let mut c = None;
    {
      let a = "bla";
      c = Some(id_str(a));
    }
    println!("{:?}", c);
  }

  // Only inline strings (&str?) seems to exhibit this.  A reference to an
  // integer (another primitive type) behaves as the Vec.
  {
    let mut c = None;
    {
      let a = 8;
      c = Some(id_u8(&a));
    }
    println!("{:?}", c);
  }
}

fn id(a: &[u8]) -> &[u8] {
  a
}

fn id_str(a: &str) -> &str {
  a
}

fn id_u8(a: &u8) -> &u8 {
  a
}
