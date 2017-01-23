pub struct Transition {
  frames: u32,
  elapsed: u32,
  goal: [f32; 2],
  step: [f32; 2],
  current: [f32; 2],
  done: bool,
}

impl Transition {
  pub fn new(start: [f32; 2], goal: [f32; 2], frames: u32) -> Self {

    Transition {
      frames: frames,
      elapsed: 0,
      goal: goal,
      step: [(goal[0] - start[0]) / (frames as f32),
             (goal[1] - start[1]) / (frames as f32)],
      current: start,
      done: false,
    }
  }

  pub fn update(&mut self) {
    self.current[0] += self.step[0];
    self.current[1] += self.step[1];
    self.elapsed += 1;

    if self.elapsed >= self.frames {
      self.current = self.goal;
      self.done = true;
    }
  }

  pub fn done(&self) -> bool {
    self.done
  }

  pub fn current(&self) -> [f32; 2] {
    self.current
  }
}
