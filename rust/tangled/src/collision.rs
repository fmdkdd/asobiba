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

#[derive(Debug)]
pub struct BoundingCircle {
  x: f32,
  y: f32,
  r: f32,
}

impl BoundingCircle {
  pub fn new(x: f32, y: f32, radius: f32) -> Self {
    BoundingCircle {
      x: x,
      y: y,
      r: radius,
    }
  }
}

impl Contains<[f32; 2]> for BoundingCircle {
  fn contains(&self, p: [f32; 2]) -> bool {
    let dx = self.x - p[0];
    let dy = self.y - p[1];
    let d = ((dx * dx) + (dy * dy)).sqrt();
    d < self.r
  }
}
