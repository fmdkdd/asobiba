#[cfg(target_os = "android")]
pub mod display_android;

#[cfg(not(target_os = "android"))]
pub mod display_glutin;

use glium::Frame;
use glium::glutin::Event;
use glium::backend::glutin_backend::GlutinFacade;

pub trait Display {
  fn can_render(&self) -> bool;
  fn log(&self, msg: &str);
  fn update(&mut self);
  fn events(&mut self) -> Vec<Event>;
  fn facade(&self) -> &GlutinFacade;
  fn frame(&self) -> Frame;
}
