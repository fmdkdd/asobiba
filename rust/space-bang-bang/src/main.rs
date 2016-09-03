#[macro_use]
extern crate glium;
#[macro_use]
extern crate imgui;

use std::time::Instant;

use glium::glutin::{Event, ElementState, VirtualKeyCode, MouseButton,
                    MouseScrollDelta, TouchPhase};
use glium::{DisplayBuild, Surface};
use imgui::ImGui;

// The state we hold for ImGUI
struct UiState {
  mouse_pos: (i32, i32),
  mouse_pressed: (bool, bool, bool),
  mouse_wheel: f32,
}

impl UiState {
  fn new() -> UiState {
    UiState {
      mouse_pos: (0,0),
      mouse_pressed: (false, false, false),
      mouse_wheel: 0.0,
    }
  }

  fn update_mouse(&mut self, imgui: &mut ImGui) {
    imgui.set_mouse_pos(self.mouse_pos.0 as f32, self.mouse_pos.1 as f32);
    imgui.set_mouse_down(&[self.mouse_pressed.0, self.mouse_pressed.1,
                           self.mouse_pressed.2, false, false]);
    imgui.set_mouse_wheel(self.mouse_wheel);

    // Need to clear the mouse wheel value for this frame after feeding to
    // ImGUI, otherwise... see what happens.
    self.mouse_wheel = 0.0;
  }
}

fn main() {
  // Init window
  let display = glium::glutin::WindowBuilder::new()
    .with_title("Space Bang Bang")
    .build_glium().unwrap();

  // Init ImGui
  let mut imgui = ImGui::init();
  let mut imgui_renderer = imgui::glium_renderer::Renderer::init(
    &mut imgui, &display).unwrap();

  let mut ui_state = UiState::new();
  let mut last_ui_time = Instant::now();

  // Init the ship
  let shape = vec![
    Vertex { position: [-0.5, -0.5] },
    Vertex { position: [ 0.0,  0.5] },
    Vertex { position: [ 0.0, -0.25] },
    Vertex { position: [ 0.5, -0.5] },
    Vertex { position: [ 0.0,  0.5] },
    Vertex { position: [ 0.0, -0.25] },
  ];

  // The ship can rotate and move on its own.
  let mut position = [0.0f32, 0.0f32];
  let mut heading: u8 = 0u8;
  const HEADING_TO_RADS: f32 = std::f32::consts::PI / (128 as f32);
  let mut acceleration = 0.0f32;

  let vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();
  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

  // We rotate the ship in the vertex shader using a projection matrix
  let r = heading as f32 * HEADING_TO_RADS;
  let mut projection = [
    [ r.cos(), r.sin(), 0.0, 0.0],
    [-r.sin(), r.cos(), 0.0, 0.0],
    [0.0, 0.0, 1.0, 0.0],
    [0.0, 0.0, 0.0, 1.0],
  ];

  let vertex_shader_src = r#"
    #version 140

    in vec2 position;

    uniform mat4 matrix;

    void main() {
        gl_Position = matrix * vec4(position, 0.0, 1.0);
    }
"#;

  let fragment_shader_src = r#"
    #version 140

    out vec4 color;

    void main() {
        color = vec4(1.0, 1.0, 1.0, 1.0);
    }
"#;

  let program = glium::Program::from_source(&display, vertex_shader_src, fragment_shader_src, None).unwrap();

  // Main loop
  'running: loop {
    for event in display.poll_events() {
      match event {
        Event::Closed
          | Event::KeyboardInput(ElementState::Pressed, _, Some(VirtualKeyCode::Escape))
          => { break 'running },

        Event::KeyboardInput(ElementState::Pressed, _, Some(vkey)) => {
          match vkey {
            VirtualKeyCode::A => heading = heading.wrapping_add(4),
            VirtualKeyCode::S => heading = heading.wrapping_sub(4),
            VirtualKeyCode::W => acceleration = 1.0,
            VirtualKeyCode::R => acceleration = -0.5,
            _ => ()
          }
        },

        Event::MouseMoved(x, y) => ui_state.mouse_pos = (x, y),
        Event::MouseInput(state, MouseButton::Left) =>
          ui_state.mouse_pressed.0 = state == ElementState::Pressed,
        Event::MouseInput(state, MouseButton::Right) =>
          ui_state.mouse_pressed.1 = state == ElementState::Pressed,
        Event::MouseInput(state, MouseButton::Middle) =>
          ui_state.mouse_pressed.2 = state == ElementState::Pressed,
        Event::MouseWheel(MouseScrollDelta::LineDelta(_, y), TouchPhase::Moved) =>
          ui_state.mouse_wheel = y,
        Event::MouseWheel(MouseScrollDelta::PixelDelta(_, y), TouchPhase::Moved) =>
          ui_state.mouse_wheel = y,

        _ => {}
      }
    }

    ui_state.update_mouse(&mut imgui);

    // Time how long we spend between frames
    let now = Instant::now();
    let delta = now - last_ui_time;
    let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;
    last_ui_time = now;

    // Create frame to draw on
    let mut frame = display.draw();

    // Clear the frame, otherwise welcome to Windows 95 error mode.
    frame.clear_color(0.0, 0.0, 0.0, 0.0);

    // Update the ship projection matrix
    let r = heading as f32 * HEADING_TO_RADS;
    projection[0][0] = r.cos();
    projection[0][1] = r.sin();
    projection[1][0] = -r.sin();
    projection[1][1] = r.cos();

    // Draw the ship
    frame.draw(&vertex_buffer, &indices, &program,
               &uniform! { matrix: projection },
               &Default::default()).unwrap();

    // Draw the GUI
    let window = display.get_window().unwrap();
    let size_points = window.get_inner_size_points().unwrap();
    let size_pixels = window.get_inner_size_pixels().unwrap();
    let ui = imgui.frame(size_points, size_pixels, delta_s);

    ui.text(format!("position: {:?}", position).into());
    ui.text(format!("heading: {}", heading).into());
    ui.text(format!("acceleration: {}", acceleration).into());

    // Tell ImGUI to render on this frame
    imgui_renderer.render(&mut frame, ui).unwrap();

    // Swap buffers
    frame.finish().unwrap();
  }
}


#[derive(Copy, Clone)]
struct Vertex {
  position: [f32; 2],
}

implement_vertex!(Vertex, position);
