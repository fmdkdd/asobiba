extern crate sdl2;
extern crate rand;
extern crate rustc_serialize;
extern crate docopt;

use std::io::prelude::*;
use std::fs::File;
use std::collections::LinkedList;
use std::time::{Instant, Duration};

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::event::Event::{KeyDown, KeyUp};
use sdl2::keyboard::Keycode;
use sdl2::render::Renderer;
use sdl2::video::Window;
use sdl2::rect::Point;

use rand::{ThreadRng, Rng};

use docopt::Docopt;

const RAM_LENGTH: usize = 0x1000;
const NUM_REGS: usize = 0x10;

struct Cpu<'a> {
  ram: [u8; RAM_LENGTH],
  v: [u8; NUM_REGS],
  pc: u16,
  i: u16,
  delay_timer: u8,
  sound_timer: u8,
  stack: LinkedList<u16>,
  asleep: bool,
  key_register: usize,

  screen: Screen<'a>,
  keyboard: Keyboard,
  rng: ThreadRng,
}

impl<'a> Cpu<'a> {
  fn new<'b>(screen: Screen<'b>, keyboard: Keyboard) -> Cpu<'b> {
    Cpu {
      ram: [0; RAM_LENGTH],
      v: [0; NUM_REGS],
      pc: 0,
      i: 0,
      delay_timer: 0,
      sound_timer: 0,
      stack: LinkedList::new(),
      asleep: false,
      key_register: 0,

      screen: screen,
      keyboard: keyboard,
      rng: rand::thread_rng(),
    }
  }

  fn reset(&mut self) {
    self.pc = 0x200;

    for i in 0..RAM_LENGTH {
      self.ram[i] = 0;
    }

    for i in 0..NUM_REGS {
      self.v[i] = 0;
    }

    self.i = 0;
    self.delay_timer = 0;
    self.sound_timer = 0;
    self.stack = LinkedList::new();
    self.asleep = false;
    self.key_register = 0;

    self.screen.clear();
    self.screen.repaint();

    self.load_font();
  }

  fn load_font(&mut self) {
    let font = [
      0xf0, 0x90, 0x90, 0x90, 0xf0, // 0
      0x20, 0x60, 0x20, 0x20, 0x70, // 1
      0xf0, 0x10, 0xf0, 0x80, 0xf0, // 2
      0xf0, 0x10, 0xf0, 0x10, 0xf0, // 3
      0x90, 0x90, 0xf0, 0x10, 0x10, // 4
      0xf0, 0x80, 0xf0, 0x10, 0xf0, // 5
      0xf0, 0x80, 0xf0, 0x90, 0xf0, // 6
      0xf0, 0x10, 0x20, 0x40, 0x40, // 7
      0xf0, 0x90, 0xf0, 0x90, 0xf0, // 8
      0xf0, 0x90, 0xf0, 0x10, 0xf0, // 9
      0xf0, 0x90, 0xf0, 0x90, 0x90, // A
      0xe0, 0x90, 0xe0, 0x90, 0xe0, // B
      0xf0, 0x80, 0x80, 0x80, 0xf0, // C
      0xe0, 0x90, 0x90, 0x90, 0xe0, // D
      0xf0, 0x80, 0xf0, 0x80, 0xf0, // E
      0xf0, 0x80, 0xf0, 0x80, 0x80  // F
    ];
    self.ram[..font.len()].copy_from_slice(&font);
  }

  fn load_rom(&mut self, rom: &[u8]) {
    for i in 0..rom.len() {
      self.ram[i + 0x200] = rom[i];
    }
  }

  fn down_key(&mut self, key: u8) {
    self.keyboard.down_key(key);

    if self.asleep {
      self.v[self.key_register] = key;
      self.asleep = false
    }
  }

  fn release_key(&mut self, key: u8) {
    self.keyboard.release_key(key);
  }

  fn step(&mut self) {
    if self.asleep { return }

    let opcode = (self.ram[self.pc as usize] as u16) << 8
      | (self.ram[(self.pc + 1) as usize] as u16);
    self.pc += 2;

    self.exec(opcode);
  }

  fn frame(&mut self) {
    for _ in 0..CYCLES_PER_FRAME {
      self.step();
    }

    if self.delay_timer > 0 {
      self.delay_timer -= 1;
    }

    if self.sound_timer > 0 {
      self.sound_timer -= 1;
    }

    self.screen.repaint();
  }

  fn is_key_down(&self, key: u8) -> bool {
    self.keyboard.is_key_down(key)
  }

  fn exec(&mut self, opcode: u16) {
    let addr = opcode & 0x0FFF;
    let x = ((opcode & 0x0F00) >> 8) as usize;
    let y = ((opcode & 0x00F0) >> 4) as usize;
    let kk = (opcode & 0x00FF) as u8;

    match opcode & 0xF000 {
      0x0000 => match opcode & 0x00FF {
        0x00 => {},

        0xE0 => self.screen.clear(),

        0xEE => self.pc = self.stack.pop_front().unwrap(),

        _ => panic!("Unknown upcode {:x}", opcode)
      },

      0x1000 => self.pc = addr,

      0x2000 => {
        self.stack.push_front(self.pc);
        self.pc = addr;
      },

      0x3000 => if self.v[x] == kk { self.pc += 2 },
      0x4000 => if self.v[x] != kk { self.pc += 2},
      0x5000 => if self.v[x] == self.v[y] { self.pc += 2 },

      0x6000 => self.v[x] = kk,
      0x7000 => self.v[x] = self.v[x].wrapping_add(kk),

      0x8000 => match opcode & 0x000F {
        0x0 => self.v[x] = self.v[y],
        0x1 => self.v[x] |= self.v[y],
        0x2 => self.v[x] &= self.v[y],
        0x3 => self.v[x] ^= self.v[y],

        0x4 => {
          let r = (self.v[x] as u16) + (self.v[y] as u16);
          self.v[0xF] = if r > 0xFF { 1 } else { 0 };
          self.v[x] = r as u8;
        },
        0x5 => {
          self.v[0xF] = if self.v[x] > self.v[y] { 1 } else { 0 };
          self.v[x] = self.v[x].wrapping_sub(self.v[y]);
        },

        0x6 => {
          self.v[0xF] = self.v[x] & 0x1;
          self.v[x] >>= 1;
        },

        0x7 => {
          self.v[0xF] = if self.v[y] > self.v[x] { 1 } else { 0 };
          self.v[y] = self.v[y].wrapping_sub(self.v[x]);
        },

        0xE => {
          self.v[0xF] = if (self.v[x] & 0x80) > 0 { 1 } else { 0 };
          self.v[x] <<= 1;
        },

        _ => panic!("Unknown upcode {:x}", opcode)
      },

      0x9000 => if self.v[x] != self.v[y] { self.pc += 2 },

      0xA000 => self.i = addr,

      0xB000 => self.pc = addr + (self.v[0] as u16),

      0xC000 => {
        let r : u8 = self.rng.gen();
        self.v[x] = r & kk;
      },

      0xD000 => {
        let n = opcode & 0x000F;

        // Build sprite
        let mut sprite = Vec::new();

        for i in (self.i)..(self.i + n) {
          let p = self.ram[i as usize];
          for b in 0..8 {
            sprite.push(if (p & (1 << (7 - b))) > 0 { true } else { false });
          }
        }

        // Draw
        self.v[0xF] = self.screen.draw_sprite(self.v[x] as usize,
                                              self.v[y] as usize,
                                              &sprite) as u8;
      },

      0xE000 => {
        match opcode & 0x00FF {
          0x9E => if self.is_key_down(self.v[x]) { self.pc += 2 },
          0xA1 => if !self.is_key_down(self.v[x]) { self.pc += 2 },

          _ => panic!("Unknown upcode {:x}", opcode)
        }
      },

      0xF000 => {
        match opcode & 0x00FF {
          0x07 => self.v[x] = self.delay_timer,
          0x15 => self.delay_timer = self.v[x],

          0x18 => self.sound_timer = self.v[x],

          0x0A => {
            self.asleep = true;
            // Keep track of the register to put the key code in.
            self.key_register = x;
          },

          0x1E => {
            let mut r = self.i as u32;
            r += self.v[x] as u32;
            self.v[0xF] = if r > 0xFFFF { 1 } else { 0 };
            self.i = r as u16;
          },

          0x29 => self.i = self.v[x] as u16 * 5,

          0x33 => {
            let h = self.v[x] / 100;
            let d = (self.v[x] % 100) / 10;
            let u = self.v[x] % 10;
            self.ram[self.i as usize] = h;
            self.ram[(self.i + 1) as usize] = d;
            self.ram[(self.i + 2) as usize] = u;
          },

          0x55 => {
            let start = self.i as usize;
            self.ram[start..(start + NUM_REGS)]
              .copy_from_slice(&self.v);
          },

          0x65 => {
            for i in 0..(x + 1) {
              self.v[i] = self.ram[self.i as usize + i];
            }
          },

          _ => panic!("Unknown upcode {:x}", opcode)
        }
      }

      _ => panic!("Unknown upcode {:x}", opcode)
    }
  }
}

const SCREEN_HEIGHT: usize = 32;
const SCREEN_WIDTH: usize = 64;
const COLOR: Color = Color::RGB(100, 100, 220);
const BLACK: Color = Color::RGB(0, 0, 0);

struct Screen<'a> {
  pixels: [bool; SCREEN_HEIGHT * SCREEN_WIDTH],
  renderer: Renderer<'a>,
}

impl<'a> Screen<'a> {
  fn new(window: Window, zoom: f32) -> Screen<'a> {
    let mut renderer = window.renderer().build().unwrap();

    renderer.set_scale(zoom, zoom).unwrap();
    renderer.clear();
    renderer.present();

    Screen {
      pixels: [false; SCREEN_HEIGHT * SCREEN_WIDTH],
      renderer: renderer,
    }
  }

  fn clear(&mut self) {
    for p in self.pixels.iter_mut() {
      *p = false
    }

    self.renderer.set_draw_color(BLACK);
    self.renderer.clear();
  }

  fn repaint(&mut self) {
    self.renderer.present();
  }

  fn draw_pixel(&mut self, p: bool, x: usize, y: usize) -> bool {
    let x = x % SCREEN_WIDTH;
    let y = y % SCREEN_HEIGHT;

    let pos = y * SCREEN_WIDTH + x;
    let collision = p && self.pixels[pos];
    self.pixels[pos] ^= p;

    if p {
      if self.pixels[pos] {
        self.renderer.set_draw_color(COLOR);
      }
      else {
        self.renderer.set_draw_color(BLACK);
      }
      let point = Point::from((x as i32, y as i32));
      self.renderer.draw_point(point).unwrap();
    }

    collision
  }

  fn draw_sprite(&mut self, x: usize, y: usize, sprite: &[bool]) -> bool {
    let width = 8;
    let height = sprite.len() / 8;
    let mut collision = false;

    for yy in 0..height {
      for xx in 0..width {
        if self.draw_pixel(sprite[yy * width + xx], x + xx, y + yy) {
          collision = true
        }
      }
    }

    collision
  }
}

const NUM_KEYS: usize = 0x10;

struct Keyboard {
  pressed_keys: [bool; NUM_KEYS],
}

impl Keyboard {
  fn new() -> Keyboard {
    Keyboard {
      pressed_keys: [false; NUM_KEYS],
    }
  }

  fn down_key(&mut self, key: u8) {
    self.pressed_keys[key as usize] = true
  }

  fn release_key(&mut self, key: u8) {
    self.pressed_keys[key as usize] = false
  }

  fn is_key_down(&self, key: u8) -> bool {
    self.pressed_keys[key as usize]
  }
}

const FRAME_NS: u32 = 1000000000 / 60; // 60Hz
const CYCLES_PER_FRAME: u64 = 10;
const FPS_REPORT_INTERVAL: u64 = 100000; // Frames to wait before reporting FPS

const USAGE: &'static str = "
A Chip-8 emulator in Rust.

Usage:
  chipers [options] <rom>
  chipers -h

Options:
  -h, --help              Show this help.
  -l, --limit             Limit frames to 60Hz.
  -z <int>, --zoom <int>  Set the zoom factor of the window [default: 10].
  -v, --verbose           Show debug information.
";

#[derive(RustcDecodable)]
struct Args {
  arg_rom: String,
  flag_limit: bool,
  flag_zoom: usize,
  flag_verbose: bool,
}

fn main() {
  // Process args
  let args: Args = Docopt::new(USAGE)
    .and_then(|d| d.decode())
    .unwrap_or_else(|e| e.exit());

  // Init SDL
  let zoom = args.flag_zoom;
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();

  let window = video_subsystem.window("chipers",
                                      (SCREEN_WIDTH * zoom) as u32,
                                      (SCREEN_HEIGHT * zoom) as u32)
    .position_centered()
    .build()
    .unwrap();

  // Init Screen
  let screen = Screen::new(window, zoom as f32);

  // Init CPU
  let mut f = File::open(args.arg_rom)
    .expect("Error opening ROM");
  let mut buf = Vec::new();
  f.read_to_end(&mut buf)
    .expect("Error reading ROM");

  let mut cpu = Cpu::new(screen, Keyboard::new());

  cpu.reset();
  cpu.load_rom(&buf);

  // Main loop
  let frame_duration = Duration::new(0, FRAME_NS);
  let mut frames = 0;
  let mut last_fps = Instant::now();
  let mut last_frame = Instant::now();

  let mut event_pump = sdl_context.event_pump().unwrap();

  'running: loop {
    for event in event_pump.poll_iter() {
      match event {
        Event::Quit {..}
        | KeyDown { keycode: Some(Keycode::Escape), .. } => {
          break 'running
        },

        KeyDown { keycode: Some(Keycode::Num1), .. } => cpu.down_key(0x1),
        KeyDown { keycode: Some(Keycode::Num2), .. } => cpu.down_key(0x2),
        KeyDown { keycode: Some(Keycode::Num3), .. } => cpu.down_key(0x3),
        KeyDown { keycode: Some(Keycode::Q), .. }    => cpu.down_key(0x4),
        KeyDown { keycode: Some(Keycode::W), .. }    => cpu.down_key(0x5),
        KeyDown { keycode: Some(Keycode::F), .. }    => cpu.down_key(0x6),
        KeyDown { keycode: Some(Keycode::A), .. }    => cpu.down_key(0x7),
        KeyDown { keycode: Some(Keycode::R), .. }    => cpu.down_key(0x8),
        KeyDown { keycode: Some(Keycode::S), .. }    => cpu.down_key(0x9),
        KeyDown { keycode: Some(Keycode::Z), .. }    => cpu.down_key(0xA),
        KeyDown { keycode: Some(Keycode::X), .. }    => cpu.down_key(0x0),
        KeyDown { keycode: Some(Keycode::C), .. }    => cpu.down_key(0xB),
        KeyDown { keycode: Some(Keycode::Num4), .. } => cpu.down_key(0xC),
        KeyDown { keycode: Some(Keycode::P), .. }    => cpu.down_key(0xD),
        KeyDown { keycode: Some(Keycode::T), .. }    => cpu.down_key(0xE),
        KeyDown { keycode: Some(Keycode::V), .. }    => cpu.down_key(0xF),

        KeyUp { keycode: Some(Keycode::Num1), .. }   => cpu.release_key(0x1),
        KeyUp { keycode: Some(Keycode::Num2), .. }   => cpu.release_key(0x2),
        KeyUp { keycode: Some(Keycode::Num3), .. }   => cpu.release_key(0x3),
        KeyUp { keycode: Some(Keycode::Q), .. }      => cpu.release_key(0x4),
        KeyUp { keycode: Some(Keycode::W), .. }      => cpu.release_key(0x5),
        KeyUp { keycode: Some(Keycode::F), .. }      => cpu.release_key(0x6),
        KeyUp { keycode: Some(Keycode::A), .. }      => cpu.release_key(0x7),
        KeyUp { keycode: Some(Keycode::R), .. }      => cpu.release_key(0x8),
        KeyUp { keycode: Some(Keycode::S), .. }      => cpu.release_key(0x9),
        KeyUp { keycode: Some(Keycode::Z), .. }      => cpu.release_key(0xA),
        KeyUp { keycode: Some(Keycode::X), .. }      => cpu.release_key(0x0),
        KeyUp { keycode: Some(Keycode::C), .. }      => cpu.release_key(0xB),
        KeyUp { keycode: Some(Keycode::Num4), .. }   => cpu.release_key(0xC),
        KeyUp { keycode: Some(Keycode::P), .. }      => cpu.release_key(0xD),
        KeyUp { keycode: Some(Keycode::T), .. }      => cpu.release_key(0xE),
        KeyUp { keycode: Some(Keycode::V), .. }      => cpu.release_key(0xF),

        _ => {}
      }
    }

    cpu.frame();

    if args.flag_limit {
      std::thread::sleep(frame_duration - last_frame.elapsed());
      last_frame = Instant::now();
    }

    if args.flag_verbose {
      frames += 1;
      if frames == FPS_REPORT_INTERVAL {
        let dt = last_fps.elapsed();
        let secs = dt.as_secs() as f64 + (dt.subsec_nanos() as f64 / 1e9);

        println!("{} frames/sec", FPS_REPORT_INTERVAL as f64 / secs);

        frames = 0;
        last_fps = Instant::now();
      }
    }
  }
}
