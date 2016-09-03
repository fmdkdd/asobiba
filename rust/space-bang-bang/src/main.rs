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

  // To hold UI debug info
  const FRAME_PERIOD_HISTORY_SIZE: usize = 128;
  let mut frame_period_history = [0f32; FRAME_PERIOD_HISTORY_SIZE];
  let mut frame_period_history_idx = 0;
  let mut avg_frame_period = 0.0;

  // Init the ship
  let shape = vec![
    Vertex { position: [-0.5,   0.5] },
    Vertex { position: [ 0.5,   0.0] },
    Vertex { position: [-0.25,  0.0] },
    Vertex { position: [-0.5,  -0.5] },
    Vertex { position: [ 0.5,   0.0] },
    Vertex { position: [-0.25,  0.0] },
  ];

  // The ship can rotate and move on its own.
  let mut position = [0.0f32, 0.0f32];
  let mut heading: u8 = 0u8;
  const HEADING_TO_RADS: f32 = std::f32::consts::PI / (128 as f32);
  let mut velocity = [0.0f32, 0.0f32];

  let vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();
  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

  // We rotate the ship in the vertex shader using a projection matrix
  let r = heading as f32 * HEADING_TO_RADS;
  const SHIP_SCALE: f32 = 10.0;
  let mut projection = [
    [ r.cos(), r.sin(), 0.0, 0.0],
    [-r.sin(), r.cos(), 0.0, 0.0],
    [0.0, 0.0, 1.0, 0.0],
    [0.0, 0.0, 0.0, SHIP_SCALE],
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

  // Keep track of player actions we need to emulate during the frame
  let mut turning_left = false;
  let mut turning_right = false;
  let mut boosting = false;
  let mut braking = false;

  // Main loop
  'running: loop {
    for event in display.poll_events() {
      match event {
        Event::Closed
          | Event::KeyboardInput(ElementState::Pressed, _, Some(VirtualKeyCode::Escape))
          => { break 'running },

        Event::KeyboardInput(ElementState::Pressed, _, Some(vkey)) => {
          match vkey {
            VirtualKeyCode::A => turning_left = true,
            VirtualKeyCode::S => turning_right = true,
            VirtualKeyCode::W => boosting = true,
            VirtualKeyCode::R => braking = true,
            _ => ()
          }
        },

        Event::KeyboardInput(ElementState::Released, _, Some(vkey)) => {
          match vkey {
            VirtualKeyCode::A => turning_left = false,
            VirtualKeyCode::S => turning_right = false,
            VirtualKeyCode::W => boosting = false,
            VirtualKeyCode::R => braking = false,
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
    let frame_period = now - last_ui_time;
    let frame_period_s = frame_period.as_secs() as f32 + frame_period.subsec_nanos() as f32 / 1_000_000_000.0;
    last_ui_time = now;

    // Create frame to draw on
    let mut frame = display.draw();

    // Clear the frame, otherwise welcome to Windows 95 error mode.
    frame.clear_color(0.0, 0.0, 0.0, 0.0);

    // Turning changes the heading
    if turning_left { heading = heading.wrapping_add(4) }
    if turning_right { heading = heading.wrapping_sub(4) }

    // Boosting increases velocity in the direction we are headed
    let heading_rad = heading as f32 * HEADING_TO_RADS;
    if boosting {
      velocity[0] += 0.01 * heading_rad.cos();
      velocity[1] += 0.01 * heading_rad.sin();
    }

    // Clamp velocity by its magnitude.  So, convert to polar and back
    {
      let mut r = f32::sqrt(velocity[0] * velocity[0] + velocity[1] * velocity[1]);
      let p = velocity[1].atan2(velocity[0]);

      // Braking reduces velocity magnitude (multiply by <1)
      if braking { r *= 0.9 }

      r = clamp(r, 0.0, 0.2);

      velocity[0] = r * p.cos();
      velocity[1] = r * p.sin();
    }

    // Update the ship position based on its velocity
    position[0] += velocity[0];
    position[1] += velocity[1];

    // Wrap around the screen
    if position[0] < -SHIP_SCALE { position[0] += 2.0 * SHIP_SCALE }
    else if position[0] > SHIP_SCALE { position[0] -= 2.0 * SHIP_SCALE }
    if position[1] < -SHIP_SCALE { position[1] += 2.0 * SHIP_SCALE }
    else if position[1] > SHIP_SCALE { position[1] -= 2.0 * SHIP_SCALE }

    // Update the ship projection matrix
    projection[0][0] = heading_rad.cos();
    projection[0][1] = heading_rad.sin();
    projection[1][0] = -heading_rad.sin();
    projection[1][1] = heading_rad.cos();
    projection[3][0] = position[0];
    projection[3][1] = position[1];

    // Draw the ship
    frame.draw(&vertex_buffer, &indices, &program,
               &uniform! { matrix: projection },
               &Default::default()).unwrap();

    // Draw the GUI
    let window = display.get_window().unwrap();
    let size_points = window.get_inner_size_points().unwrap();
    let size_pixels = window.get_inner_size_pixels().unwrap();
    let ui = imgui.frame(size_points, size_pixels, frame_period_s);

    ui.text(format!("position: {:?}", position).into());
    ui.text(format!("heading: {}", heading).into());
    ui.text(format!("velocity: {:?}", velocity).into());

    frame_period_history[frame_period_history_idx % FRAME_PERIOD_HISTORY_SIZE] =
      frame_period_s * 1000.0;
    frame_period_history_idx += 1;

    ui.plot_histogram(
      format!("frame period (ms)\navg: {:.3}ms",
              avg_frame_period).into(), &frame_period_history)
      .values_offset(frame_period_history_idx)
      .graph_size(imgui::ImVec2::new(FRAME_PERIOD_HISTORY_SIZE as f32, 40.0))
      .scale_min(0.0)
      .scale_max(30.0)
      .build();

    avg_frame_period = frame_period_history.iter().fold(0f32, |a, &b| a + b)
      / FRAME_PERIOD_HISTORY_SIZE as f32;

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

fn clamp(x: f32, min: f32, max: f32) -> f32 {
  // Hmm, have to use min/max specific to floats to handle NaN properly.
  // An alternative would be to use a number type that /cannot/ be NaN, since it
  // does not make sense in our context.
  f32::min(f32::max(x, min), max)
}
