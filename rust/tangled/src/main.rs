// TODO: Handle more properly mapping to/from GL coordinates from/to screen

mod collision;
mod display;
mod graph;
mod transition;

#[macro_use]
extern crate glium;
extern crate rand;
extern crate time;

use glium::{Program, Surface, VertexBuffer};
use glium::glutin::{Event, ElementState, MouseButton, Touch, TouchPhase, VirtualKeyCode};

// use time::SteadyTime;

use collision::Contains;
use display::Display;
use graph::{Graph, Node};

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

struct Input {
  x: f64,
  y: f64,
  down: bool,
  selected_node: Option<usize>,
}

pub fn main() {
  let mut rng = rand::thread_rng();

  // Construct the graph
  let mut g = Graph::new();

  g.add_node(-3.0, -3.0, 0.5);
  g.add_node(-3.0,  3.0, 0.5);
  g.add_node( 3.0, -3.0, 0.5);
  g.add_node( 3.0,  3.0, 0.5);

  g.add_edge(0, 1);
  g.add_edge(0, 2);
  g.add_edge(0, 3);
  g.add_edge(1, 2);

  // Construct the rendering context
  let mut window = create_window("Tangled");
  // let mut last_frame = SteadyTime::now();

  // Input handling
  let mut input = Input {
    x: 0.0,
    y: 0.0,
    down: false,
    selected_node: None,
  };

  // Main loop
  'running: loop {
    // Let the window update its state.  Necessary for Android to tear-down or
    // setup the window as needed.
    window.update();

    if window.can_render() {
      // TODO: display on screen instead, in debug version
      // let dt = SteadyTime::now() - last_frame;
      // window.log(&format!("{:.3}ms", dt.num_microseconds().unwrap() as f32 / 1000f32));
      // last_frame = SteadyTime::now();

      // Create frame to draw on
      let mut frame = window.frame();

      for event in window.events() {
        match event {
          Event::Closed |
          Event::KeyboardInput(.., Some(VirtualKeyCode::Escape)) => {
            // Cleanup the frame
            // FIXME: or is there a way to get the window dimensions for picking
            // before getting a frame?
            frame.finish().unwrap();
            break 'running
          }

          Event::MouseMoved(x, y) => {
            // Convert pixel coordinates [0, width] to GL [-1, +1]
            let dims = frame.get_dimensions();
            let width = dims.0 as f64;
            let height = dims.1 as f64;
            input.x = (x as f64) / width * 2.0 - 1.0;
            input.y = (height - (y as f64)) / height * 2.0 - 1.0;
          }

          Event::MouseInput(ElementState::Pressed, MouseButton::Left) => {
            input.down = true;
          }

          Event::Touch(Touch { phase: TouchPhase::Started, location: xy, .. }) => {
            // Convert pixel coordinates [0, width] to GL [-1, +1]
            let dims = frame.get_dimensions();
            let width = dims.0 as f64;
            let height = dims.1 as f64;
            input.x = (xy.0 as f64) / width * 2.0 - 1.0;
            input.y = (height - (xy.1 as f64)) / height * 2.0 - 1.0;
            input.down = true;
          }

          _ => {}
        }
      }

      // User clicked.  Check if it's on a node.
      if input.down {
        // Only take the first node
        let touched_node = g.nodes()
          .find(|n| n.bounding_circle(5.0).contains([input.x as f32, input.y as f32]))
          .map(|n| n.id);

        match touched_node {
          Some(id) => {
            match input.selected_node {
              // Had we selected a node before?
              Some(selected) => {
                // Is it the same one?  If so, unselect it
                if selected == id {
                  input.selected_node = None;
                }
                // If not, swap the two nodes
                else {
                  g.swap_nodes(selected, id, 30);
                  input.selected_node = None;
                }
              }

              // Otherwise, select this one
              None => {
                input.selected_node = Some(id);
              }
            }
          }

          None => {}
        }

        input.down = false;
      }

      // Update transitions and add a new one when empty
      g.update_transitions();
      // if !g.has_transitions() {
      //   let r = rand::sample(&mut rng, 0..g.nodes().len(), 2);
      //   g.swap_nodes(r[0], r[1], 30);
      // }

      // Clear the frame, otherwise welcome to Windows 95 error mode.
      frame.clear_color(0.3, 0.3, 0.3, 1.0);

      // Draw edges below nodes

      // FIXME: should not recreate these each frame
      let edge_program = Program::from_source(
        window.facade(),
        include_str!("shader/edge.v.glsl"),
        include_str!("shader/edge.f.glsl"),
        None).unwrap();

      let node_program = Program::from_source(
        window.facade(),
        include_str!("shader/edge.v.glsl"),
        include_str!("shader/node.f.glsl"),
        None).unwrap();

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
                   &edge_program,
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
      let node_vbo = VertexBuffer::immutable(
        window.facade(),
        &[
          Vertex { position: [-1.0, -1.0] },
          Vertex { position: [-1.0,  1.0] },
          Vertex { position: [ 1.0, -1.0] },
          Vertex { position: [ 1.0,  1.0] },
        ]).unwrap();

      let node_center_vbo = VertexBuffer::immutable(
        window.facade(),
        &[
          Vertex { position: [0.0, 0.0] },
        ]).unwrap();

      for n in g.nodes() {
        let projection = [
          [ 1.0, 0.0, 0.0, 0.0],
          [ 0.0, 1.0, 0.0, 0.0],
          [ 0.0, 0.0, 1.0, 0.0],
          [ n.x, n.y, 0.0, 5.0f32],
        ];

        // Compute the center of the node in screen coordinates for the fragment
        // shader
        let dims = frame.get_dimensions();
        let width = dims.0 as f32;
        let height = dims.1 as f32;
        let center = [(n.x / 10.0 + 0.5) * width,
                      (n.y / 10.0 + 0.5) * height];

        // Compute radius size in screen coordinates
        let r = n.radius / 10.0 * width;

        let color = if input.selected_node.is_some()
          && input.selected_node.unwrap() == n.id {
            [1.0, 0.0, 0.0f32]
          } else { [0.0, 0.0, 0.0f32] };

        frame.draw(&node_vbo,
                   glium::index::NoIndices(glium::index::PrimitiveType::TriangleStrip),
                   &node_program,
                   &uniform! {
                     projection: projection,
                     color: color,
                     radius: r,
                     center: center,
                   },
                   &Default::default()).unwrap();

        frame.draw(&node_center_vbo,
                   glium::index::NoIndices(glium::index::PrimitiveType::Points),
                   &node_program,
                   &uniform! {
                     projection: projection,
                     color: [1.0, 0.0, 0.0f32],
                     radius: 5.0f32,
                     center: center,
                   },
                   &glium::draw_parameters::DrawParameters {
                     point_size: Some(10.0f32),
                     .. Default::default()
                   }).unwrap();
      }

      // Draw mouse
      let projection = [
        [ 1.0, 0.0, 0.0, 0.0],
        [ 0.0, 1.0, 0.0, 0.0],
        [ 0.0, 0.0, 1.0, 0.0],
        [ input.x as f32, input.y as f32, 0.0, 1.0f32],
      ];

      frame.draw(&node_center_vbo,
                 glium::index::NoIndices(glium::index::PrimitiveType::Points),
                 &node_program,
                 &uniform! {
                   projection: projection,
                   color: [1.0, 0.0, 0.0f32],
                 },
                 &glium::draw_parameters::DrawParameters {
                   point_size: Some(10.0f32),
                   .. Default::default()
                 }).unwrap();

      // Swap buffers
      frame.finish().unwrap();
    }
  }
}
