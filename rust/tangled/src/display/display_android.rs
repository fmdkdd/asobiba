extern crate android_glue;

use std::sync::mpsc;

use glium::{self, glutin, DisplayBuild};
use glium::glutin::Event;
use glium::backend::glutin_backend::GlutinFacade;

use display::Display;

pub struct AndroidWindow<'a> {
  rx: mpsc::Receiver<android_glue::Event>,
  title: &'a str,
  can_render: bool,
  display: Option<GlutinFacade>,
}

impl<'a> AndroidWindow<'a> {
  pub fn new(title: &'a str) -> Self {
    let (tx, rx) = mpsc::channel();
    android_glue::add_sender_missing(tx);

    AndroidWindow {
      rx: rx,
      title: title,
      can_render: false,
      display: None,
    }
  }
}

impl<'a> Display for AndroidWindow<'a> {
  fn log(&self, msg: &str) {
    android_glue::write_log(msg);
  }

  fn facade(&self) -> &GlutinFacade {
    self.display.as_ref().unwrap()
  }

  fn frame(&self) -> glium::Frame {
    // Should panic only if frame() is called when can_render() is false
    self.display.as_ref().unwrap().draw()
  }

  fn can_render(&self) -> bool {
    self.can_render
  }

  fn update(&mut self) {
    match self.rx.try_recv() {
      Err(_) => { /* No event, do nothing */ }
      Ok(ev) => {
        self.log(&format!("android event {:?}", ev));

        match ev {
          android_glue::Event::InitWindow => {
            self.display = Some(
              glutin::WindowBuilder::new()
                .with_title(self.title)
                .with_gl(glutin::GlRequest::Specific(glutin::Api::OpenGlEs, (2, 0)))
                .with_vsync()
                .build_glium().unwrap());

            self.can_render = true;
          }

          android_glue::Event::TermWindow => {
            self.display = None;
            self.can_render = false;
          }

          _ => {}
        }
      }
    }
  }

  fn events(&mut self) -> Vec<Event> {
    // FIXME: maybe try to get as many events as possible instead of just one
    match self.display {
      None => Vec::new(),
      Some(ref display) => display.poll_events().collect()
    }
  }
}
