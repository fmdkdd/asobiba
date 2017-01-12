#[macro_use]
extern crate glium;

extern crate android_glue;

use std::sync::mpsc;

use glium::{DisplayBuild, Surface};
use glium::glutin::{Event, Touch, TouchPhase, VirtualKeyCode};

pub fn main() {
  // Init android
  let (tx, rx) = mpsc::channel();
  android_glue::add_sender_missing(tx);

  let display = glium::glutin::WindowBuilder::new()
    .with_title("Tangled")
    .with_gl(glium::glutin::GlRequest::GlThenGles {
      opengles_version: (2, 0),
      opengl_version: (2, 1),
    })
    .with_vsync()
    .build_glium().unwrap();

  let mut touched = false;
  let mut animate = false;

  // Main loop
  'running: loop {
    match rx.try_recv() {
      Err(_) => { /* No event, do nothing */ }
      Ok(ev) => {
        android_glue::write_log(&format!("android event {:?}", ev));

        match ev {
          android_glue::Event::InitWindow => {
            animate = true;
          }

          android_glue::Event::TermWindow => {
            animate = false;
          }

          _ => {}
        }
      }
    }

    if animate {
      for event in display.poll_events() {
        match event {
          Event::Closed | Event::KeyboardInput( .., Some(VirtualKeyCode::Escape))
            => { break 'running },

          Event::Touch ( Touch { phase: TouchPhase::Started, ..} ) => {
            touched = !touched;
          }

          _ => {}
        }
      }

      // Create frame to draw on
      let mut frame = display.draw();

      // Clear the frame, otherwise welcome to Windows 95 error mode.
      if touched {
        frame.clear_color(0.02, 0.02, 1.0, 0.0);
      } else {
        frame.clear_color(1.00, 0.02, 0.024, 0.0);
      }

      // Swap buffers
      frame.finish().unwrap();
    }
  }
}
