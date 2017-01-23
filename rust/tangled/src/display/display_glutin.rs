use glium::{self, glutin, DisplayBuild};
use glium::glutin::Event;
use glium::backend::glutin_backend::GlutinFacade;

use display::Display;

pub struct GlutinWindow {
  display: GlutinFacade,
}

impl GlutinWindow {
  pub fn new(title: &str) -> Self {
    let display = glutin::WindowBuilder::new()
      .with_title(title)
      .with_gl(glutin::GlRequest::Specific(glutin::Api::OpenGl, (2, 1)))
      .with_vsync()
      .build_glium()
      .unwrap();

    GlutinWindow { display: display }
  }
}

impl Display for GlutinWindow {
  fn log(&self, msg: &str) {
    println!("{}", msg);
  }

  fn update(&mut self) {}

  fn events(&mut self) -> Vec<Event> {
    self.display.poll_events().collect()
  }

  fn can_render(&self) -> bool {
    true
  }

  fn facade(&self) -> &GlutinFacade {
    &self.display
  }

  fn frame(&self) -> glium::Frame {
    self.display.draw()
  }
}
