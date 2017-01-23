use std::slice::{Iter, IterMut};

use collision::BoundingCircle;
use transition::Transition;

pub struct Node {
  pub id: usize,
  pub x: f32,
  pub y: f32,
  pub radius: f32,
  transition: Option<Transition>,
}

impl Node {
  fn new(id: usize, x: f32, y: f32, radius: f32) -> Self {
    Node {
      id: id,
      x: x,
      y: y,
      radius: radius,
      transition: None,
    }
  }

  pub fn xy(&self) -> [f32; 2] {
    [self.x, self.y]
  }

  pub fn bounding_circle(&self, zoom: f32) -> BoundingCircle {
    BoundingCircle::new(self.x / zoom, self.y / zoom, (self.radius + 0.1) / zoom)
  }
}

// Transition-related
impl Node {
  pub fn init_transition(&mut self, to: [f32; 2], frames: u32) {
    self.transition = Some(Transition::new(self.xy(), to, frames));
  }

  pub fn update_transition(&mut self) {
    let mut clean = false;

    match self.transition {
      Some(ref mut t) => {
        t.update();
        self.x = t.current()[0];
        self.y = t.current()[1];

        // Cleanup
        if t.done() {
          clean = true;
        }
      }

      None => {}
    }

    if clean {
      self.transition = None;
    }
  }

  pub fn has_transition(&self) -> bool {
    self.transition.is_some()
  }
}

pub struct Edge {
  n1: usize,
  n2: usize,
}

impl Edge {
  pub fn new(n1: usize, n2: usize) -> Self {
    Edge {
      n1: n1,
      n2: n2,
    }
  }
}

pub struct Graph {
  nodes: Vec<Node>,
  edges: Vec<Edge>,
}

impl Graph {
  pub fn new() -> Graph {
    Graph {
      nodes: Vec::new(),
      edges: Vec::new(),
    }
  }

  pub fn add_node(&mut self, x: f32, y: f32, radius: f32) {
    let id = self.nodes.len();
    self.nodes.push(Node::new(id, x, y, radius));
  }

  pub fn add_edge(&mut self, n1: usize, n2: usize) {
    assert!(n1 < self.nodes.len());
    assert!(n2 < self.nodes.len());
    self.edges.push(Edge::new(n1, n2));
  }

  pub fn edge_n1(&self, e: &Edge) -> &Node {
    &self.nodes[e.n1]
  }

  pub fn edge_n2(&self, e: &Edge) -> &Node {
    &self.nodes[e.n2]
  }

  pub fn nodes<'a>(&'a self) -> Iter<'a, Node> {
    self.nodes.iter()
  }

  pub fn nodes_mut<'a>(&'a mut self) -> IterMut<'a, Node> {
    self.nodes.iter_mut()
  }

  pub fn edges<'a>(&'a self) -> Iter<'a, Edge> {
    self.edges.iter()
  }
}

// Transition-related
impl Graph {
  pub fn update_transitions(&mut self) {
    for n in self.nodes.iter_mut() {
      n.update_transition();
    }
  }

  pub fn has_transitions(&self) -> bool {
    self.nodes.iter().any(|n| n.has_transition())
  }

  pub fn swap_nodes(&mut self, n1: usize, n2: usize, frames: u32) {
    {
      let to = self.nodes[n2].xy();
      let mut n = &mut self.nodes[n1];
      n.init_transition(to, frames);
    }
    {
      let to = self.nodes[n1].xy();
      let mut n = &mut self.nodes[n2];
      n.init_transition(to, frames);
    }
  }
}
