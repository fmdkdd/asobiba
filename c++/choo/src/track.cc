#include "track.h"

#include <glad/glad.h>

void Track::init() { pointCount = 0; }

void Track::add(float x, float y) {
  ASSERT(pointCount < MAX_POINTS);
  Vec2 &p = points[pointCount];
  p.x = x;
  p.y = y;
  pointCount++;
}

void Track::render() const {
  glColor4f(0.0f, 1.0f, 1.0f, 1.0f);
  glLineWidth(1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glBegin(GL_LINE_STRIP);
  for (usize i = 0; i < pointCount; ++i) {
    const Vec2 &p = points[i];
    glVertex2f(p.x, p.y);
  }
  glEnd();
}

float Track::length() const {
  float len = 0.0f;
  for (usize i = 1; i < pointCount; ++i) {
    const Vec2 &p0 = points[i - 1];
    const Vec2 &p1 = points[i];
    len += p0.distance(p1);
  }
  return len;
}

Vec2 Track::positionAtLength(float l) const {
  ASSERT(!isEmpty());
  for (usize i = 1; i < pointCount; ++i) {
    const Vec2 &p0 = points[i - 1];
    const Vec2 &p1 = points[i];
    const float d = p0.distance(p1);
    if (l > d)
      l -= d;
    else {
      const float remainingRatio = l / d;
      return p0 + (p0.vectorTo(p1) * remainingRatio);
    }
  }
  return points[pointCount - 1];
}
