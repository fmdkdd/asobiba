// Glium test following the tutorial at
// https://tomaka.github.io/glium/book/index.html

#[macro_use]
extern crate glium;

use glium::{DisplayBuild, Surface};

#[derive(Copy, Clone)]
struct Vertex {
  position: [f32; 2],
}

implement_vertex!(Vertex, position);

fn point(vec: &mut Vec<Vertex>, x: u32, y: u32) {
  let x = x as f32;
  let x1 = x + 1.0;
  let y = y as f32;
  let y1 = y + 1.0;
  vec.push(Vertex { position: [  x,  y ] });
  vec.push(Vertex { position: [ x1,  y ] });
  vec.push(Vertex { position: [  x, y1 ] });
  vec.push(Vertex { position: [  x, y1 ] });
  vec.push(Vertex { position: [ x1,  y ] });
  vec.push(Vertex { position: [ x1, y1 ] });
}

fn main() {
  let display = glium::glutin::WindowBuilder::new()
    .with_dimensions(640, 320)
    .with_vsync()
    .build_glium().unwrap();

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
      color = vec4(1.0, 0.7, 1.0, 1.0);
    }
  "#;

  let program = glium::Program::from_source(
    &display, vertex_shader_src, fragment_shader_src, None)
    .unwrap();

  let mut shape = Vec::new();
  point(&mut shape, 0, 0);
  point(&mut shape, 63,0);
  point(&mut shape, 63,31);
  point(&mut shape, 0,31);
  let vertex_buffer = glium::VertexBuffer::new(&display, &shape).unwrap();

  let indices = glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList);

  let mut shape2 = Vec::new();
  point(&mut shape2, 61,0);
  let vertex_buffer2 = glium::VertexBuffer::new(&display, &shape2).unwrap();

  let uniforms = uniform! {
    matrix: [
      [ 2.0/64.0, 0.0,      0.0, 0.0],
      [ 0.0,      2.0/32.0, 0.0, 0.0],
      [ 0.0,      0.0,      1.0, 0.0],
      [-1.0,     -1.0,      0.0, 1.0f32],
    ],
  };

  loop {

    let mut target = display.draw();
    target.draw(&vertex_buffer, &indices, &program, &uniforms,
                &Default::default()).unwrap();
    target.finish().unwrap();

    std::thread::sleep_ms(500);

    let mut target = display.draw();
    target.draw(&vertex_buffer2, &indices, &program, &uniforms,
                &Default::default()).unwrap();
    target.finish().unwrap();

    std::thread::sleep_ms(500);

    for ev in display.poll_events() {
      match ev {
        glium::glutin::Event::Closed => return,

        _ => ()
      }
    }
  }
}
