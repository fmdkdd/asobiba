#[macro_use]
extern crate glium;
#[macro_use]
extern crate imgui;

use std::io::BufReader;
use std::fs::File;
use std::time::Instant;

use glium::glutin::{Event, ElementState, VirtualKeyCode, MouseButton,
                    MouseScrollDelta, TouchPhase};

use glium::index::PrimitiveType;
use glium::{DisplayBuild, Surface, VertexBuffer, IndexBuffer};
use glium::texture::{UncompressedFloatFormat, MipmapsOption};
use glium::texture::texture2d::Texture2d;
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};

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
    .with_vsync()
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
  let mut avg_frame_period;

  // Init the ship
  let ship_stl_file = File::open("../assets/ship.stl").unwrap();
  let mut ship_stl_reader = BufReader::new(ship_stl_file);
  let ship_stl = stl::read_stl(&mut ship_stl_reader).unwrap();

  let mut shape: Vec<Vertex> = Vec::new();
  for i in 0..ship_stl.header.num_triangles {
    shape.push(ship_stl.triangles[i as usize].v1.into());
    shape.push(ship_stl.triangles[i as usize].v2.into());
    shape.push(ship_stl.triangles[i as usize].v3.into());
  }

  // The ship can rotate and move on its own.
  let mut position = [0.0f32, 0.0f32];
  let mut heading: u8 = 0u8;
  const HEADING_TO_RADS: f32 = std::f32::consts::PI / (128 as f32);
  let mut velocity = [0.0f32, 0.0f32];

  let vertex_buffer = VertexBuffer::new(&display, &shape).unwrap();
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

  let mut view = [
    [ 1.0, 0.0, 0.0, 0.0],
    [ 0.0, 1.0, 0.0, 0.0],
    [ 0.0, 0.0, 1.0, 0.0],
    [ 0.0, 0.0, 0.0, 1.0f32],
  ];

  let vertex_shader_src = r#"
    #version 120

    attribute vec3 position;

    uniform mat4 matrix;
    uniform mat4 view;

    void main() {
        gl_Position = view * matrix * vec4(position, 1.0);
    }
"#;

  let fragment_shader_src = r#"
    #version 120

    void main() {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
"#;

  let program = glium::Program::from_source(&display, vertex_shader_src, fragment_shader_src, None).unwrap();

  // We want to render to a low resolution framebuffer and use it as a texture
  // that we will draw to the screen afterwards

  // This is the texture resolution.  Since the resolution can be changed
  // through the GUI, the texture is created on the fly in the main loop.
  let mut virtual_resolution = (320i32, 200);

  // The quad to draw the texture on
  let quad_vertices = [
    Vertex { position: [-1.0, -1.0, 0.0], tex_coords: [0.0, 0.0] },
    Vertex { position: [-1.0,  1.0, 0.0], tex_coords: [0.0, 1.0] },
    Vertex { position: [ 1.0,  1.0, 0.0], tex_coords: [1.0, 1.0] },
    Vertex { position: [ 1.0, -1.0, 0.0], tex_coords: [1.0, 0.0] }
  ];

  let quad_vertex_buffer = VertexBuffer::immutable(&display, &quad_vertices).unwrap();
  let quad_index_buffer = IndexBuffer::immutable(
    &display, PrimitiveType::TriangleStrip, &[1u16, 2, 0, 3]).unwrap();

  let quad_vertex_shader_src = r#"
    #version 120

    attribute vec3 position;
    attribute vec2 tex_coords;
    varying vec2 v_tex_coords;

    uniform mat4 perspective;

    void main() {
        v_tex_coords = tex_coords;
        gl_Position = perspective * vec4(position, 1.0);
    }
"#;

  let quad_fragment_shader_src = r#"
    #version 120

    varying vec2 v_tex_coords;

    uniform sampler2D tex;

    void main() {
        gl_FragColor = texture2D(tex, v_tex_coords);
    }
"#;

  let quad_program = glium::Program::from_source(&display, quad_vertex_shader_src, quad_fragment_shader_src, None).unwrap();

  let mut perspective = [
    [ 1.0, 0.0, 0.0, 0.0],
    [ 0.0, 1.0, 0.0, 0.0],
    [ 0.0, 0.0, 1.0, 0.0],
    [ 0.0, 0.0, 0.0, 1.0f32],
  ];

  // Keep track of player actions we need to emulate during the frame
  let mut turning_left = false;
  let mut turning_right = false;
  let mut boosting = false;
  let mut braking = false;

  // Gameplay tweakables
  let mut turn_speed = 4i32;
  let mut boost_speed = 0.01f32;
  let mut brake_factor = 0.9f32;
  let mut max_velocity = 0.2f32;

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

    // Create the framebuffer
    let texture = Texture2d::empty_with_format(&display,
                                               UncompressedFloatFormat::U8U8U8U8,
                                               MipmapsOption::NoMipmap,
                                               virtual_resolution.0 as u32,
                                               virtual_resolution.1 as u32).unwrap();

    let mut framebuffer = glium::framebuffer::SimpleFrameBuffer::new(&display, &texture).unwrap();

    // Clear the frame, otherwise welcome to Windows 95 error mode.
    framebuffer.clear_color(0.02, 0.02, 0.024, 0.0);

    // Turning changes the heading
    if turning_left { heading = heading.wrapping_add(turn_speed as u8) }
    if turning_right { heading = heading.wrapping_sub(turn_speed as u8) }

    // Boosting increases velocity in the direction we are headed
    let heading_rad = heading as f32 * HEADING_TO_RADS;
    if boosting {
      velocity[0] += boost_speed * heading_rad.cos();
      velocity[1] += boost_speed * heading_rad.sin();
    }

    // Clamp velocity by its magnitude.  So, convert to polar and back
    {
      let mut r = f32::sqrt(velocity[0] * velocity[0] + velocity[1] * velocity[1]);
      let p = velocity[1].atan2(velocity[0]);

      // Braking reduces velocity magnitude (multiply by <1)
      if braking { r *= brake_factor }

      r = clamp(r, 0.0, max_velocity);

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

    // Adjust for aspect ratio of framebuffer
    view[0][0] = virtual_resolution.1 as f32 / virtual_resolution.0 as f32;

    framebuffer.draw(&vertex_buffer, &indices, &program,
                     &uniform! {
                       matrix: projection,
                       view: view,
                     },
                     &Default::default()).unwrap();

    // Draw the framebuffer to the actual screen

    // This stretches the virtual framebuffer to fill the screen while
    // maintaining the aspect ratio of the framebuffer.
    let (width, height) = frame.get_dimensions();
    perspective[0][0] = virtual_resolution.0 as f32 / width as f32;
    perspective[1][1] = virtual_resolution.1 as f32 / height as f32;
    perspective[3][3] = f32::max(perspective[0][0], perspective[1][1]);

    frame.clear_color(0.0, 0.0, 0.0, 0.0);
    frame.draw(&quad_vertex_buffer,
               &quad_index_buffer,
               &quad_program,
               &uniform! {
                 tex: texture.sampled()
                   .minify_filter(MinifySamplerFilter::Nearest)
                   .magnify_filter(MagnifySamplerFilter::Nearest),
                 perspective: perspective,
               }, &Default::default()).unwrap();


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

    avg_frame_period = frame_period_history.iter().fold(0f32, |a, &b| a + b)
      / FRAME_PERIOD_HISTORY_SIZE as f32;

    ui.plot_histogram(
      format!("frame period (ms)\navg: {:.3}ms",
              avg_frame_period).into(), &frame_period_history)
      .values_offset(frame_period_history_idx)
      .graph_size(imgui::ImVec2::new(FRAME_PERIOD_HISTORY_SIZE as f32, 40.0))
      .scale_min(0.0)
      .scale_max(30.0)
      .build();

    ui.slider_int(im_str!("Virtual width"),
                  &mut virtual_resolution.0,
                  1, 2000).build();
    ui.slider_int(im_str!("Virtual height"),
                  &mut virtual_resolution.1,
                  1, 2000).build();

    ui.slider_int(im_str!("Turn speed"),
                  &mut turn_speed,
                  1, 64).build();
    ui.slider_float(im_str!("Boost speed"),
                    &mut boost_speed,
                    0.0, 1.0).build();
    ui.slider_float(im_str!("Brake factor"),
                    &mut brake_factor,
                    0.0, 1.0).build();
    ui.slider_float(im_str!("Max velocity"),
                    &mut max_velocity,
                    0.0, 1.0).build();

    // Tell ImGUI to render on this frame
    imgui_renderer.render(&mut frame, ui).unwrap();

    // Swap buffers
    frame.finish().unwrap();
  }
}


#[derive(Copy, Clone)]
struct Vertex {
  position: [f32; 3],
  tex_coords: [f32; 2],
}
implement_vertex!(Vertex, position, tex_coords);

impl From<[f32; 3]> for Vertex {
  fn from(v: [f32; 3]) -> Self {
    Vertex {
      position: v,
      tex_coords: [0.0, 0.0],
    }
  }
}

fn clamp(x: f32, min: f32, max: f32) -> f32 {
  // Hmm, have to use min/max specific to floats to handle NaN properly.
  // An alternative would be to use a number type that /cannot/ be NaN, since it
  // does not make sense in our context.
  f32::min(f32::max(x, min), max)
}


mod stl {
  // Stolen from https://github.com/eholk/rust-stl just to make the vertices
  // inside the Triangle struct public, otherwise you cannot use them!

  extern crate byteorder;

  use std::io::{Result, ErrorKind, Error};
  use self::byteorder::{ReadBytesExt, LittleEndian};

  pub struct Triangle {
    normal: [f32; 3],
    pub v1: [f32; 3],
    pub v2: [f32; 3],
    pub v3: [f32; 3],
    attr_byte_count: u16
  }

  fn point_eq(lhs: [f32; 3], rhs: [f32; 3]) -> bool {
    lhs[0] == rhs[0] && lhs[1] == rhs[1] && lhs[2] == rhs[2]
  }

  impl PartialEq for Triangle {
    fn eq(&self, rhs: &Triangle) -> bool {
      point_eq(self.normal, rhs.normal)
        && point_eq(self.v1, rhs.v1)
        && point_eq(self.v2, rhs.v2)
        && point_eq(self.v3, rhs.v3)
        && self.attr_byte_count == rhs.attr_byte_count
    }
  }

  impl Eq for Triangle {}

  pub struct BinaryStlHeader {
    pub header: [u8; 80],
    pub num_triangles: u32
  }

  pub struct BinaryStlFile {
    pub header: BinaryStlHeader,
    pub triangles: Vec<Triangle>
  }

  fn read_point<T: ReadBytesExt>(input: &mut T) -> Result<[f32; 3]> {
    let x1 = try!(input.read_f32::<LittleEndian>());
    let x2 = try!(input.read_f32::<LittleEndian>());
    let x3 = try!(input.read_f32::<LittleEndian>());

    Ok([x1, x2, x3])
  }

  fn read_triangle<T: ReadBytesExt>(input: &mut T) -> Result<Triangle> {
    let normal = try!(read_point(input));
    let v1 = try!(read_point(input));
    let v2 = try!(read_point(input));
    let v3 = try!(read_point(input));
    let attr_count = try!(input.read_u16::<LittleEndian>());

    Ok(Triangle { normal: normal,
                  v1: v1, v2: v2, v3: v3,
                  attr_byte_count: attr_count })
  }

  fn read_header<T: ReadBytesExt>(input: &mut T) -> Result<BinaryStlHeader> {
    let mut header = [0u8; 80];

    match input.read(&mut header) {
      Ok(n) => if n == header.len() {
        ()
      }
      else {
        return Err(Error::new(ErrorKind::Other,
                              "Couldn't read STL header"));
      },
      Err(e) => return Err(e)
    };

    let num_triangles = try!(input.read_u32::<LittleEndian>());

    Ok(BinaryStlHeader{ header: header, num_triangles: num_triangles })
  }

  pub fn read_stl<T: ReadBytesExt>(input: &mut T) -> Result<BinaryStlFile> {

    // read the header
    let header = try!(read_header(input));

    let mut triangles = Vec::new();
    for _ in 0 .. header.num_triangles {
      triangles.push(try!(read_triangle(input)));
    }

    Ok(BinaryStlFile {
      header: header,
      triangles: triangles
    })
  }



}
