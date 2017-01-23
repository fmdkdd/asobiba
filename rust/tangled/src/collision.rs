pub trait Contains<T> {
  fn contains(&self, t: T) -> bool;
}

#[derive(Debug)]
pub struct BoundingBox {
  top_left: [f32; 2],
  bottom_right: [f32; 2],
}

impl BoundingBox {
  pub fn new(top_left: [f32; 2], bottom_right: [f32; 2]) -> Self {
    BoundingBox {
      top_left: top_left,
      bottom_right: bottom_right,
    }
  }
}

impl Contains<[f32; 2]> for BoundingBox {
  fn contains(&self, p: [f32; 2]) -> bool {
    self.top_left[0] < p[0]
      && p[0] < self.bottom_right[0]
      && p[1] < self.top_left[1]
      && self.bottom_right[1] < p[1]
  }
}
