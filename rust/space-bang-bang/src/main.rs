#[macro_use]
extern crate glium;
#[macro_use]
extern crate imgui;

use std::time::Instant;

use glium::glutin::{Event, ElementState, VirtualKeyCode, MouseButton,
                    MouseScrollDelta, TouchPhase};
use glium::{DisplayBuild, Surface};
use imgui::ImGui;

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

  fn update_mouse(&self, imgui: &mut ImGui) {
    imgui.set_mouse_pos(self.mouse_pos.0 as f32, self.mouse_pos.1 as f32);
    imgui.set_mouse_down(&[self.mouse_pressed.0, self.mouse_pressed.1,
                           self.mouse_pressed.2, false, false]);
    imgui.set_mouse_wheel(self.mouse_wheel);
  }
}

fn main() {
  let display = glium::glutin::WindowBuilder::new()
    .with_title("Space Bang Bang")
    .build_glium().unwrap();

  // Init ImGui
  let mut imgui = ImGui::init();
  let mut imgui_renderer = imgui::glium_renderer::Renderer::init(
    &mut imgui, &display).unwrap();

  let mut ui_state = UiState::new();
  let mut last_ui_time = Instant::now();

  // Main loop
  'running: loop {
    for event in display.poll_events() {
      match event {
        Event::Closed
          | Event::KeyboardInput(ElementState::Pressed, _, Some(VirtualKeyCode::Escape))
          => { break 'running },

        // Event::KeyboardInput(ElementState::Pressed, _, Some(vkey)) => {
        //   match vkey {
        //     _ => ()
        //   }
        // },

        // Event::KeyboardInput(ElementState::Released, _, Some(vkey)) => {
        //   match vkey {
        //     _ => ()
        //   }
        // },

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
    ui_state.mouse_wheel = 0.0; // Clear value for this frame

    // Create frame and signal ImGui
    let now = Instant::now();
    let delta = now - last_ui_time;
    let delta_s = delta.as_secs() as f32 + delta.subsec_nanos() as f32 / 1_000_000_000.0;
    last_ui_time = now;

    let mut frame = display.draw();
    frame.clear_color(0.0, 0.0, 0.0, 0.0);

    let window = display.get_window().unwrap();
    let size_points = window.get_inner_size_points().unwrap();
    let size_pixels = window.get_inner_size_pixels().unwrap();
    let ui = imgui.frame(size_points, size_pixels, delta_s);

    ui.text(format!("Hello!").into());

    imgui_renderer.render(&mut frame, ui).unwrap();
    frame.finish().unwrap();
  }
}
