mod display;
mod graph;

#[macro_use]
extern crate glium;

extern crate rand;
extern crate time;

use glium::{Program, Surface, VertexBuffer};
use glium::glutin::{Event, Touch, TouchPhase, VirtualKeyCode};

use time::SteadyTime;

use display::Display;
use graph::{Graph, Node};

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

#[cfg(target_os = "android")]
fn create_window(title: &str) -> display::display_android::AndroidWindow {
  display::display_android::AndroidWindow::new(title)
}

#[cfg(not(target_os = "android"))]
fn create_window(title: &str) -> display::display_glutin::GlutinWindow {
  display::display_glutin::GlutinWindow::new(title)
}

pub fn main() {
  let mut rng = rand::thread_rng();

  // Construct the graph
  let mut g = Graph::new();

  g.add_node(-3.0, -3.0);
  g.add_node(-3.0, -2.0);
  g.add_node( 2.0, -3.0);
  g.add_node( 2.0, -2.0);

  g.add_edge(0, 1);
  g.add_edge(0, 2);
  g.add_edge(0, 3);
  g.add_edge(1, 2);

  let mut transitions = vec![
    Transition::new(&nodes, 0, nodes[1], 30),
    Transition::new(&nodes, 1, nodes[0], 30),
  ];

  // Construct the rendering context
  let mut window = create_window("Tangled");
  let mut last_frame = SteadyTime::now();
  let mut touched = false;

  // Main loop
  'running: loop {
    let edge_proj = [
      [ 1.0, 0.0, 0.0, 0.0],
      [ 0.0, 1.0, 0.0, 0.0],
      [ 0.0, 0.0, 1.0, 0.0],
      [ 0.0, 0.0, 0.0, 5.0f32],
    ];

    for event in window.events() {
      match event {
        Event::Closed |
        Event::KeyboardInput(.., Some(VirtualKeyCode::Escape)) => {
          break 'running
        }

        Event::MouseInput(..) |
        Event::Touch(Touch { phase: TouchPhase::Started, .. }) => {
          touched = !touched
        }

        _ => {}
      }
    }

    if window.can_render() {
      let node_program = Program::from_source(
        window.facade(),
        include_str!("shader/edge.v.glsl"),
        include_str!("shader/edge.f.glsl"),
        None).unwrap();

      let node_vbo = VertexBuffer::immutable(
        window.facade(),
        &[
          Vertex { position: [-1.0, -1.0] },
          Vertex { position: [-1.0,  1.0] },
          Vertex { position: [ 1.0, -1.0] },
          Vertex { position: [ 1.0,  1.0] },
        ]).unwrap();

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
      window.log(&format!("{:.3}ms", dt.num_microseconds().unwrap() as f32 / 1000f32));
      last_frame = SteadyTime::now();

      // Create frame to draw on
      let mut frame = window.frame();

      // Clear the frame, otherwise welcome to Windows 95 error mode.
      if touched {
        frame.clear_color(0.3, 0.2, 0.4, 1.0);
      } else {
        frame.clear_color(0.4, 0.2, 0.3, 1.0);
      }

      // Draw edges below nodes
      for e in g.edges() {
        let vbo = VertexBuffer::immutable(window.facade(), &[
          Vertex { position: g.node(e.n1).into() },
          Vertex { position: g.node(e.n2).into() },
        ]).unwrap();

        frame.draw(&vbo,
                   glium::index::NoIndices(glium::index::PrimitiveType::LinesList),
                   &node_program,
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
      for n in g.nodes() {
        let projection = [
          [ 1.0, 0.0, 0.0, 0.0],
          [ 0.0, 1.0, 0.0, 0.0],
          [ 0.0, 0.0, 1.0, 0.0],
          [ n.x, n.y, 0.0, 5.0f32],
        ];

        frame.draw(&node_vbo,
                   glium::index::NoIndices(glium::index::PrimitiveType::TriangleStrip),
                   &node_program,
                   &uniform! { projection: projection, },
                   &Default::default()).unwrap();
      }

      // Swap buffers
      frame.finish().unwrap();
    }
  }
}
