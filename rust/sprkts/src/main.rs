extern crate ws;
extern crate bincode;
extern crate rustc_serialize;
extern crate time;

// use std::rc::Rc;
// use std::cell::Cell;

use std::sync::mpsc;

use bincode::rustc_serialize::{encode, decode};
use rustc_serialize::Encodable;

struct Client {
  out: ws::Sender,
  input_mask: u8,
  ship: Ship,
}

impl Client {
  fn new(out: ws::Sender, ship: Ship) -> Client {
    println!("shipx: {}", ship.x);
    Client {
      out: out,
      input_mask: 0,
      ship: ship,
    }
  }
}

impl ws::Handler for Client {
  fn on_open(&mut self, _: ws::Handshake) -> ws::Result<()> {
    println!("Websocket client connected");

    Ok(())
  }

  fn on_close(&mut self, code: ws::CloseCode, reason: &str) {
    println!("Websocket client left: {:?} {}", code, reason)
  }

  fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
    let msg_vec : Vec<u8> = msg.into_data();
    println!("Websocket client msg: '{:?}'", msg_vec);

    self.input_mask = msg_vec[0];

    if self.input_mask & 1 > 0 {
      self.ship.move_by(-1, 0);
    }

    if self.input_mask & 4 > 0 {
      self.ship.move_by(1, 0);
    }

    if self.input_mask & 2 > 0 {
      self.ship.move_by(0, -1);
    }

    if self.input_mask & 8 > 0 {
      self.ship.move_by(0, 1);
    }

    let msg = Message {
      x: self.ship.x,
      y: self.ship.y,
    };

    let limit = bincode::SizeLimit::Bounded(1000);
    self.out.send(encode(&msg, limit).unwrap().as_slice())
  }
}

#[derive(RustcEncodable)]
struct Message {
  x: i32,
  y: i32,
}

struct Game;

impl ws::Factory for Game {
  type Handler = Client;

  fn connection_made(&mut self, out: ws::Sender) -> Client {
    let ship = Ship::new();
    let c = Client::new(out, ship);
    // self.clients.push(c);
    c
  }
}

fn main() {
  let game = Game;
  let socket = ws::WebSocket::new(game)
    .expect("Error creating WebSocket");
  let broadcaster = socket.broadcaster();

  std::thread::spawn(|| {
    socket.listen("localhost:12345")
      .expect("Error listening at 12345");
  });

  let periodic = timer_periodic(20);
  let mut last_time = time::PreciseTime::now();

  loop {
    let now = time::PreciseTime::now();
    let dt = last_time.to(now);
    last_time = now;

    // broadcaster.send(Game)
    // println!("tick: {}", dt);

    periodic.recv();
  }
}

// All the game messages
// enum Message {
//   Hello,
//   ShipPosition(i32, i32),
// }

struct Ship {
  x: i32,
  y: i32,
}

impl Ship {
  fn new() -> Ship {
    Ship { x: 0, y: 0 }
  }

  fn move_by(&mut self, dx: i32, dy: i32) {
    self.x += dx;
    self.y += dy;
  }
}

fn timer_periodic(ms: u64) -> mpsc::Receiver<()> {
  let (tx, rx) = mpsc::sync_channel(1);
  std::thread::spawn(move || {
    loop {
      std::thread::sleep(std::time::Duration::from_millis(ms));
      tx.send(()).unwrap();
    }
  });
  rx
}
