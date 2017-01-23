mod display;
mod transition;
mod graph;

#[macro_use]
extern crate glium;
extern crate rand;
extern crate time;

use glium::{Program, Surface, VertexBuffer};
use glium::glutin::{Event, Touch, TouchPhase, VirtualKeyCode};

use time::SteadyTime;

use display::Display;
use graph::Graph;

#[derive(Copy, Clone)]
struct Vertex {
  position: [f32; 2],
}
implement_vertex!(Vertex, position);

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
  g.add_node(-3.0,  3.0);
  g.add_node( 3.0, -3.0);
  g.add_node( 3.0,  3.0);

  g.add_edge(0, 1);
  g.add_edge(0, 2);
  g.add_edge(0, 3);
  g.add_edge(1, 2);

  // Construct the rendering context
  let mut window = create_window("Tangled");
  let mut last_frame = SteadyTime::now();
  let mut touched = false;

  // Main loop
  'running: loop {
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
      // Update transitions and add a new one when empty
      g.update_transitions();
      if !g.has_transitions() {
        let r = rand::sample(&mut rng, 0..g.nodes().len(), 2);
        g.swap_nodes(r[0], r[1], 30);
      }

      // FIXME: should not recreate these each frame
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
      let edge_proj = [
        [ 1.0, 0.0, 0.0, 0.0],
        [ 0.0, 1.0, 0.0, 0.0],
        [ 0.0, 0.0, 1.0, 0.0],
        [ 0.0, 0.0, 0.0, 5.0f32],
      ];

      for e in g.edges() {
        let vbo = VertexBuffer::immutable(window.facade(), &[
          Vertex { position: g.edge_n1(e).xy() },
          Vertex { position: g.edge_n2(e).xy() },
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
        let xy = n.xy();
        let projection = [
          [ 1.0, 0.0, 0.0, 0.0],
          [ 0.0, 1.0, 0.0, 0.0],
          [ 0.0, 0.0, 1.0, 0.0],
          [ xy[0], xy[1], 0.0, 5.0f32],
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
