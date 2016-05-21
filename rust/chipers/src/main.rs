extern crate sdl2;
extern crate rand;

use std::io::prelude::*;
use std::fs::File;
use std::collections::LinkedList;
use std::env;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::Renderer;
use sdl2::video::Window;
use sdl2::rect::Point;

use rand::{ThreadRng, Rng};

const RAM_LENGTH: usize = 0x1000;
const NUM_REGS: usize = 0x10;

struct Cpu<'a> {
  ram: [u8; RAM_LENGTH],
  v: [u8; NUM_REGS],
  pc: u16,
  i: u16,
  stack: LinkedList<u16>,

  screen: Screen<'a>,
  rng: ThreadRng,
}

impl<'a> Cpu<'a> {
  fn new<'b>(screen: Screen<'b>) -> Cpu<'b> {
    Cpu {
      ram: [0; RAM_LENGTH],
      v: [0; NUM_REGS],
      pc: 0,
      i: 0,
      stack: LinkedList::new(),
      screen: screen,
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
  }

  fn load_rom(&mut self, rom: &[u8]) {
    for i in 0..rom.len() {
      self.ram[i + 0x200] = rom[i];
    }
  }

  fn step(&mut self) {
    let opcode = (self.ram[self.pc as usize] as u16) << 8
      | (self.ram[(self.pc + 1) as usize] as u16);
    self.pc += 2;

    self.exec(opcode);
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
          let r = self.v[x] as u16 + self.v[y] as u16;
          self.v[0xF] = if r > 0xFF { 1 } else { 0 };
          self.v[x] = r as u8;
        },
        0x5 => {
          let r = self.v[x] as i16 - self.v[y] as i16;
          self.v[0xF] = if r > 0x0 { 1 } else { 0 };
          self.v[x] = r as u8;
        },

        0x6 => {
          self.v[0xF] = self.v[x] & 0x1;
          self.v[x] >>= 1;
        },

        0x7 => {
          let r = self.v[y] as i16 - self.v[x] as i16;
          self.v[0xF] = if r > 0x0 { 1 } else { 0 };
          self.v[x] = r as u8;
        },

        0x8 => {
          self.v[0xF] = if (self.v[x] & 0x80) > 0 { 1 } else { 0 };
          self.v[x] <<= 1;
        },

        _ => panic!("Unknown upcode {:x}", opcode)
      },

      0x9000 => if self.v[x] != self.v[y] { self.pc += 2 },

      0xA000 => self.i = addr,

      0xB000 => self.pc = addr + self.v[0] as u16,

      0xC000 => {
        let r : u8 = self.rng.gen();
        self.v[x] = r & kk;
      },

      0xD000 => {
        let n = opcode & 0x000F;

        // Build sprite
        let mut sprite = Vec::new();

        for i in 0..n {
          let mut p = self.ram[(self.i + i) as usize];
          for _ in 0..8 {
            sprite.push(if (p & 1) > 0 { true } else { false });
            p >>= 1;
          }
        }

        self.v[0xF] = self.screen.draw_sprite(self.v[x] as usize,
                                              self.v[y] as usize,
                                              &sprite) as u8;

        // FIXME: throttle
        self.screen.repaint();
      },

      0xE000 => {
        // FIXME: keyboard stuff
        println!("Keyboard stuff");
      },

      0xF000 => {
        match opcode & 0x00FF {
          0x1E => {
            let mut r = self.i as u32;
            r += self.v[x] as u32;
            self.v[0xF] = if r > 0xFFFF { 1 } else { 0 };
            self.i = r as u16;
          }

          _ => panic!("Unknown upcode {:x}", opcode)
        }
      }

      _ => panic!("Unknown upcode {:x}", opcode)
    }
  }
}

const SCREEN_HEIGHT: usize = 32;
const SCREEN_WIDTH: usize = 64;
const SCREEN_SCALE: usize = 5;
const COLOR: Color = Color::RGB(100, 100, 220);
const BLACK: Color = Color::RGB(0, 0, 0);

struct Screen<'a> {
  pixels: [bool; SCREEN_HEIGHT * SCREEN_WIDTH],
  renderer: Renderer<'a>,
}

impl<'a> Screen<'a> {
  fn new(window: Window) -> Screen<'a> {
    let mut renderer = window.renderer().build().unwrap();

    renderer.set_scale(SCREEN_SCALE as f32,
                       SCREEN_SCALE as f32).unwrap();
    renderer.clear();
    renderer.present();

    Screen {
      pixels: [false; SCREEN_HEIGHT * SCREEN_WIDTH],
      renderer: renderer,
    }
  }

  fn clear(&mut self) {
    self.renderer.clear();
  }

  fn repaint(&mut self) {
    self.renderer.present();
  }

  fn draw_pixel(&mut self, p: bool, x: usize, y: usize) -> bool {
    if x > SCREEN_WIDTH { return false };
    if y > SCREEN_HEIGHT { return false };

    let pos = y * SCREEN_WIDTH + x;
    let changed = self.pixels[pos] != p;
    let collision = p && self.pixels[pos];
    self.pixels[pos] ^= p;

    if changed {
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

fn main() {
  // Init SDL
  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();

  let window = video_subsystem.window("chipers",
                                      (SCREEN_WIDTH * SCREEN_SCALE) as u32,
                                      (SCREEN_HEIGHT * SCREEN_SCALE) as u32)
    .position_centered()
    .build()
    .unwrap();

  // Init Screen
  let screen = Screen::new(window);

  // Init CPU
  let args : Vec<String> = env::args().collect();

  let mut f = File::open(args[1].clone())
    .expect("Error opening ROM");
  let mut buf = Vec::new();
  f.read_to_end(&mut buf)
    .expect("Error reading ROM");

  let mut cpu = Cpu::new(screen);

  cpu.reset();
  cpu.load_rom(&buf);

  // Main loop
  let mut event_pump = sdl_context.event_pump().unwrap();

  'running: loop {
    for event in event_pump.poll_iter() {
      match event {
        Event::Quit {..}
        | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
          break 'running
        },

        _ => {}
      }
    }

    cpu.step();
  }
}
