use graph::Node;

pub struct Transition<'a> {
  frames: u32,
  elapsed: u32,
  // start: [f32; 2],
  node: &'a mut Node,
  goal: [f32; 2],
  step: [f32; 2],
  current: [f32; 2],
  done: bool,
}

impl<'a> Transition<'a> {
  pub fn new(node: &'a mut Node, goal: [f32; 2], frames: u32) -> Self {
    let step = [(goal[0] - node.x) / (frames as f32),
                (goal[1] - node.y) / (frames as f32)];

    Transition {
      frames: frames,
      elapsed: 0,
      // start: start,
      node: node,
      goal: goal,
      step: step,
      current: [0.0, 0.0f32],
      done: false,
    }
  }

  pub fn update(&mut self) {
    self.node.x += self.step[0];
    self.node.y += self.step[1];
    self.elapsed += 1;

    if self.elapsed >= self.frames {
      self.current[0] = self.goal[0];
      self.current[1] = self.goal[1];
      self.done = true;
    }
  }

  pub fn done(&self) -> bool {
    self.done
  }

  // pub fn current(&self) -> [f32; 2] {
  //   self.current
  // }
}
