#[macro_use]
extern crate glium;

use glium::{DisplayBuild, Surface};
use glium::glutin::{Event, VirtualKeyCode};

pub fn main() {
  let display = glium::glutin::WindowBuilder::new()
    .with_title("Tangled")
    .with_gl(glium::glutin::GlRequest::GlThenGles {
      opengles_version: (2, 0),
      opengl_version: (2, 1),
    })
    .with_vsync()
    .build_glium().unwrap();

  // Main loop
  'running: loop {
    for event in display.poll_events() {
      match event {
        Event::Closed | Event::KeyboardInput( .., Some(VirtualKeyCode::Escape))
          => { break 'running },

        _ => {}
      }
    }

    // Create frame to draw on
    let mut frame = display.draw();

    // Clear the frame, otherwise welcome to Windows 95 error mode.
    frame.clear_color(0.02, 0.02, 0.024, 0.0);

    // Swap buffers
    frame.finish().unwrap();
  }
}
