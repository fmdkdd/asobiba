use std::slice::{Iter, IterMut};
use std::mem;

use transition::Transition;

pub struct Node {
  pub x: f32,
  pub y: f32,
  transition: Option<Transition>,
}

impl Node {
  pub fn new(x: f32, y: f32) -> Self {
    Node {
      x: x,
      y: y,
      transition: None,
    }
  }
}

impl Node {
  pub fn init_transition(&mut self, to: [f32; 2], frames: u32) {
    self.transition = Some(Transition::new(self.into(), to, frames));
  }

  pub fn update_transition(&mut self) -> bool {
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

    self.transition.is_some()
  }
}

impl<'a> From<&'a Node> for [f32; 2] {
  fn from(n: &Node) -> Self {
    [n.x, n.y]
  }
}

impl<'a> From<&'a mut Node> for [f32; 2] {
  fn from(n: &mut Node) -> Self {
    [n.x, n.y]
  }
}

pub struct Edge {
  pub n1: usize,
  pub n2: usize,
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

  pub fn add_node(&mut self, x: f32, y: f32) {
    self.nodes.push(Node::new(x, y));
  }

  pub fn add_edge(&mut self, n1: usize, n2: usize) {
    assert!(n1 < self.nodes.len());
    assert!(n2 < self.nodes.len());
    self.edges.push(Edge::new(n1, n2));
  }

  pub fn node(&self, idx: usize) -> &Node {
    &self.nodes[idx]
  }

  pub fn node_mut(&mut self, idx: usize) -> &mut Node {
    &mut self.nodes[idx]
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

  pub fn nodes_len(&self) -> usize {
    self.nodes.len()
  }
}
