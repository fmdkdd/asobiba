extern crate ws;

// use std::rc::Rc;
// use std::cell::Cell;

struct Client {
  out: ws::Sender,
  input_mask: u8,
  ship: Box<Ship>,
}

impl Client {
  fn new(out : ws::Sender, ship: Ship) -> Client {
    Client { out: out,
             input_mask: 0,
             ship: Box::new(ship)
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

    // if input_mask[0] & 1 == 1 {
    //   self.ship.move_by(-1, 0);
    //   println!("x: {}", self.ship.x)
    // }

    Ok(())
  }
}

struct Game {
  clients: Vec<Client>,
  // ships: Vec<Ship>,
}

impl ws::Factory for Game {
  type Handler = Client;

  fn connection_made(&mut self, out: ws::Sender) -> Client {
    let ship = Ship::new();
    let c = Client::new(out, ship);
    // let c = Client { out: out, ship: Box::new(ship) };
    c
  }
}

fn main() {
  // ws::listen("localhost:12345", |out| {
  //   Client { out: out, ship: Ship::new() }
  // })
  //   .expect("Failed to create WebSocket")

  let game = Game { clients: vec![] };

  let socket = ws::WebSocket::new(game)
    .expect("Error creating WebSocket");
  socket.listen("localhost:12345")
    .expect("Error listening at 12345");
}

// All the game messages
// enum Message {
//   Hello,
//   ShipPosition(i32, i32),
// }

// Handles serialization and deserialization of messages.
// trait Messenger {
//   fn send(p: Player, m: Message);
//   // should subscribe to messages as well?
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
