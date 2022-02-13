#include <glad/glad.h>

#include "config.h"
#include "gfx.h"
#include "network.h"

void Network::init() {
  pointCount = 0;
  edgeCount = 0;
}

void Network::render() const {
  auto color = config::trackColor;
  glColor4f(color.x, color.y, color.z, color.w);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  // Draw points
  auto pointResolution = config::trackPointResolution;
  auto pointRadius = config::trackPointRadius;

  for (usize i = 0; i < pointCount; ++i) {
    const Vec2i &p = points[i];
    drawCircle(Vec2f(p.x, p.y), pointRadius, pointResolution);
  }

  // Draw edges
  auto edgeWidth = config::trackLineWidth;

  for (usize i = 0; i < edgeCount; ++i) {
    const Edge &e = edges[i];
    ASSERT(e.from < pointCount);
    ASSERT(e.to < pointCount);
    const Vec2i &p0 = points[e.from];
    const Vec2i &p1 = points[e.to];

    Vec2f ps[2] = {Vec2f(p0.x, p0.y), Vec2f(p1.x, p1.y)};
    drawLine(ps, 2, edgeWidth);
  }
}

PointId Network::addPoint(Vec2i p) {
  ASSERT(pointCount < MAX_POINTS);
  points[pointCount] = p;
  PointId id = pointCount;
  pointCount++;
  return id;
}

void Network::addEdge(PointId from, PointId to) {
  ASSERT(edgeCount < MAX_EDGES);
  ASSERT(from < MAX_POINTS);
  ASSERT(to < MAX_POINTS);

  Edge &e = edges[edgeCount];
  e.from = from;
  e.to = to;

  edgeCount++;
}

Optional<PointId> Network::getClosestPoint(Vec2i p, float maxDistance) const {
  float bestDistance = maxDistance;
  Optional<PointId> bestCandidate;

  Vec2f a = Vec2f(p.x, p.y);

  for (usize i = 0; i < pointCount; ++i) {
    Vec2f b = Vec2f(points[i].x, points[i].y);
    float d = a.distance(b);
    if (d < bestDistance) {
      bestCandidate = Optional<PointId>(i);
      bestDistance = d;
    }
  }

  return bestCandidate;
}

struct Queue {
  static const usize MAX_POINTS = 256;

  PointId points[MAX_POINTS];
  u32 writeIndex;
  u32 readIndex;

  void init() {
    writeIndex = 0;
    readIndex = 0;
  }

  void push(PointId p) {
    ASSERT(writeIndex < MAX_POINTS);
    points[writeIndex++] = p;
  }

  void pushUnique(PointId p) {
    for (u32 i = 0; i < writeIndex; ++i) {
      if (points[i] == p)
        return;
    }

    ASSERT(writeIndex < MAX_POINTS);
    points[writeIndex++] = p;
  }

  PointId pop() {
    ASSERT(readIndex < writeIndex);
    PointId p = points[readIndex++];
    return p;
  }

  u32 size() const { return writeIndex - readIndex; }
};

struct Stack {
  static const usize MAX_POINTS = 256;

  PointId points[MAX_POINTS];
  u32 size;

  void init() { size = 0; }

  void push(PointId p) {
    ASSERT(size < MAX_POINTS);
    points[size++] = p;
  }

  PointId pop() {
    ASSERT(size > 0);
    PointId p = points[--size];
    return p;
  }

  bool isEmpty() { return size == 0; }
};

void Network::getShortestPath(PointId from, PointId to, Path *path) const {
  if (from == to) {
    path->pointCount = 0;
    return;
  }

  Queue queue;
  u32 dist[Path::MAX_POINTS];
  PointId prev[Path::MAX_POINTS];
  bool visited[Path::MAX_POINTS];

  for (usize i = 0; i < pointCount; ++i) {
    dist[i] = -1;
    visited[i] = false;
  }

  queue.init();
  dist[from] = 0;
  prev[from] = from;
  queue.push(from);

  while (queue.size() > 0) {
    PointId current = queue.pop();
    visited[current] = true;

    for (usize i = 0; i < edgeCount; ++i) {
      const Edge &e = edges[i];
      PointId neighbor;
      if (current == e.from) {
        neighbor = e.to;
      } else if (current == e.to) {
        neighbor = e.from;
      } else {
        continue;
      }

      if (visited[neighbor] == false) {
        u32 d = dist[current] + getPoint(current).distance(getPoint(neighbor));
        if (d < dist[neighbor]) {
          dist[neighbor] = d;
          prev[neighbor] = current;
        }

        queue.push(neighbor);
      }
    }
  }

  // No connection between FROM and TO
  if (dist[to] == UINT32_MAX) {
    path->pointCount = 0;
    return;
  }

  ASSERT(dist[to] < UINT32_MAX);

  Stack stack;
  stack.init();

  PointId current = to;
  while (current != from) {
    stack.push(current);
    current = prev[current];
  }
  stack.push(from);

  path->pointCount = stack.size;
  path->network = this;
  for (u32 i = 0; !stack.isEmpty(); ++i) {
    path->points[i] = stack.pop();
  }
}

u32 Path::length() const {
  u32 len = 0;

  for (u32 i=1; i < pointCount; ++i) {
    const Vec2i &a = network->getPoint(points[i - 1]);
    const Vec2i &b = network->getPoint(points[i]);
    len += a.distance(b);
  }

  return len;
}
