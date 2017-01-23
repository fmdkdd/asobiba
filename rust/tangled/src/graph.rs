use std::slice::{Iter, IterMut};

use transition::Transition;

pub struct Node {
  xy: [f32; 2],
  transition: Option<Transition>,
}

impl Node {
  pub fn new(x: f32, y: f32) -> Self {
    Node {
      xy: [x, y],
      transition: None,
    }
  }
}

impl Node {
  pub fn xy(&self) -> [f32; 2] {
    self.xy
  }

  pub fn init_transition(&mut self, to: [f32; 2], frames: u32) {
    self.transition = Some(Transition::new(self.xy, to, frames));
  }

  pub fn update_transition(&mut self) -> bool {
    let mut clean = false;

    match self.transition {
      Some(ref mut t) => {
        t.update();
        self.xy = t.current();

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

  pub fn add_node(&mut self, x: f32, y: f32) {
    self.nodes.push(Node::new(x, y));
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
