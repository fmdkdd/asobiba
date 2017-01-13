#[macro_use]
extern crate glium;

extern crate android_glue;
extern crate rand;
extern crate time;

use std::sync::mpsc;

use glium::{DisplayBuild, Program, Surface, VertexBuffer};
use glium::glutin::{Event, Touch, TouchPhase, VirtualKeyCode};

use time::SteadyTime;

#[derive(Copy, Clone)]
struct Vertex {
  position: [f32; 2],
}
implement_vertex!(Vertex, position);

struct Transition {
  node_idx: usize,
  frames: u32,
  elapsed: u32,
  target: [f32; 2],
  step: [f32; 2],
  done: bool,
}

impl Transition {
  fn new(nodes: &Vec<[f32; 2]>, node_idx: usize, target: [f32; 2], frames: u32) -> Self {
    let n = nodes[node_idx];

    // println!("{} {} {}", target[0], n[0], (target[0] - n[0]) / (frames as f32));
    // println!("{} {} {}", target[1], n[1], (target[1] - n[1]) / (frames as f32));

    Transition {
      node_idx: node_idx,
      frames: frames,
      elapsed: 0,
      target: target,
      step: [(target[0] - n[0]) / (frames as f32),
             (target[1] - n[1]) / (frames as f32)],
      done: false,
    }
  }

  fn update(&mut self, nodes: &mut Vec<[f32; 2]>) {
    let mut n = &mut nodes[self.node_idx];

    n[0] += self.step[0];
    n[1] += self.step[1];
    self.elapsed += 1;

    if self.elapsed >= self.frames {
      n[0] = self.target[0];
      n[1] = self.target[1];
      self.done = true;
    }
  }
}

pub fn main() {
  let mut rng = rand::thread_rng();

  // Init android
  let (tx, rx) = mpsc::channel();
  android_glue::add_sender_missing(tx);

  let mut touched = false;
  let mut animate = false;

  let mut nodes = vec![
    [-3.0, -3.0],
    [-3.0,  2.0],
    [ 2.0, -3.0],
    [ 2.0,  2.0f32],
  ];

  let edges = vec![
    (0, 1),
    (0, 2),
    (0, 3),
    (1, 2),
  ];

  let mut transitions = vec![
    Transition::new(&nodes, 0, nodes[1], 30),
    Transition::new(&nodes, 1, nodes[0], 30),
  ];

  let mut display = None;
  let mut node_program = None;
  let mut edge_program = None;
  let mut node_vbo = None;

  let mut last_frame = SteadyTime::now();

  // Main loop
  'running: loop {
    match rx.try_recv() {
      Err(_) => { /* No event, do nothing */ }
      Ok(ev) => {
        android_glue::write_log(&format!("android event {:?}", ev));

        match ev {
          android_glue::Event::InitWindow => {
            animate = true;

            let disp = glium::glutin::WindowBuilder::new()
              .with_title("Tangled")
              .with_gl(glium::glutin::GlRequest::GlThenGles {
                opengles_version: (2, 0),
                opengl_version: (2, 1),
              })
              .with_vsync()
              .build_glium().unwrap();

            node_program = Some(Program::from_source(
              &disp,
              include_str!("shader/edge.v.glsl"),
              include_str!("shader/edge.f.glsl"),
              None).unwrap());

            node_vbo = Some(VertexBuffer::immutable(&disp, &[
              Vertex { position: [-1.0, -1.0] },
              Vertex { position: [-1.0,  1.0] },
              Vertex { position: [ 1.0, -1.0] },
              Vertex { position: [ 1.0,  1.0] },
            ]).unwrap());

            edge_program = Some(Program::from_source(
              &disp,
              include_str!("shader/edge.v.glsl"),
              include_str!("shader/edge.f.glsl"),
              None).unwrap());

            display = Some(disp);
          }

          android_glue::Event::TermWindow => {
            animate = false;

            display = None;
            node_program = None;
            // node_vbo = None;
            // edge_program = None;
          }

          _ => {}
        }
      }
    }

    let edge_proj = [
      [ 1.0, 0.0, 0.0, 0.0],
      [ 0.0, 1.0, 0.0, 0.0],
      [ 0.0, 0.0, 1.0, 0.0],
      [ 0.0, 0.0, 0.0, 5.0f32],
    ];

    if animate {
      let display = display.as_ref().unwrap();
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

      // Update transitions
      for t in transitions.iter_mut() {
        t.update(&mut nodes);
      }
      transitions.retain(|t| t.done == false);
      if transitions.len() == 0 {
        let n = rand::sample(&mut rng, 0..nodes.len(), 2);
        transitions.push(Transition::new(&nodes, n[0], nodes[n[1]], 30));
        transitions.push(Transition::new(&nodes, n[1], nodes[n[0]], 30));
      }

      let dt = SteadyTime::now() - last_frame;
      android_glue::write_log(&format!("{:.3}ms", dt.num_microseconds().unwrap() as f32 / 1000f32));
      last_frame = SteadyTime::now();


      // Create frame to draw on
      let mut frame = display.draw();

      // Clear the frame, otherwise welcome to Windows 95 error mode.
      frame.clear_color(0.3, 0.2, 0.4, 1.0);

      // Draw edges below nodes
      for e in edges.iter() {
        let vbo = VertexBuffer::immutable(display, &[
          Vertex { position: nodes[e.0] },
          Vertex { position: nodes[e.1] },
        ]).unwrap();

        frame.draw(&vbo,
                   glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
                   node_program.as_ref().unwrap(),
                   &uniform! { projection: edge_proj, },
                   &glium::draw_parameters::DrawParameters {
                     // Can't use this mode in GLES, but line_width is applied
                     // anyway in fill mode.
                     // polygon_mode: glium::draw_parameters::PolygonMode::Line,
                     line_width: Some(8.0),
                     .. Default::default()
                   }).unwrap();
      }

      // Draw nodes
      for n in nodes.iter() {
        let projection = [
          [ 1.0, 0.0, 0.0, 0.0],
          [ 0.0, 1.0, 0.0, 0.0],
          [ 0.0, 0.0, 1.0, 0.0],
          [ n[0], n[1], 0.0, 5.0f32],
        ];

        frame.draw(node_vbo.as_ref().unwrap(),
                   glium::index::NoIndices(glium::index::PrimitiveType::TriangleStrip),
                   node_program.as_ref().unwrap(),
                   &uniform! { projection: projection, },
                   &Default::default()).unwrap();
      }

      // Swap buffers
      frame.finish().unwrap();
    }
  }
}
