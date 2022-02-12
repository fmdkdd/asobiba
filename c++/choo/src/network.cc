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
    const Point &p = points[i];
    drawCircle(Vec2f(p.x, p.y), pointRadius, pointResolution);
  }

  // Draw edges
  auto edgeWidth = config::trackLineWidth;

  for (usize i = 0; i < edgeCount; ++i) {
    const Edge &e = edges[i];
    ASSERT(e.from < pointCount);
    ASSERT(e.to < pointCount);
    const Point &p0 = points[e.from];
    const Point &p1 = points[e.to];

    Vec2f ps[2] = {Vec2f(p0.x, p0.y), Vec2f(p1.x, p1.y)};
    drawLine(ps, 2, edgeWidth);
  }
}

Network::PointId Network::addPoint(Point p) {
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
