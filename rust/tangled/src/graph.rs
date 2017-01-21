use std::slice::Iter;

pub struct Node {
  pub x: f32,
  pub y: f32,
}

impl Node {
  pub fn new(x: f32, y: f32) -> Self {
    Node {
      x: x,
      y: y,
    }
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

struct Edge {
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

  pub fn node(&mut self, idx: usize) -> &Node {
    &self.nodes[idx]
  }

  pub fn nodes<'a>(&'a self) -> Iter<'a, Node> {
    self.nodes.iter()
  }

  pub fn edges<'a>(&'a self) -> Iter<'a, Edge> {
    self.edges.iter()
  }

  pub fn nodes_len(&self) -> usize {
    self.nodes.len()
  }
}
