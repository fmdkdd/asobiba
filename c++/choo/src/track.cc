#include "track.h"

#include <glad/glad.h>

void Track::init(u32 id) {
  this->id = id;
  pointCount = 0;
}

void Track::add(float x, float y) {
  ASSERT(pointCount < MAX_POINTS);
  Vec2f &p = points[pointCount];
  p.x = x;
  p.y = y;
  pointCount++;
}

void Track::update() {}

void Track::render() const {
  glColor4f(0.0f, 1.0f, 1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  if (pointCount < 2)
    return;

  float lineWidth = 0.01f;
  glBegin(GL_TRIANGLES);

  Vec2f ab;
  Vec2f a0;
  Vec2f a1;

  {
    const Vec2f a = points[0];
    const Vec2f b = points[1];

    ab = a.vectorTo(b);
    const Vec2f normalAB = ab.ortho().normalized();
    const Vec2f dAB = (normalAB * lineWidth);
    a0 = a - dAB;
    a1 = a + dAB;
  }

  for (usize i = 2; i < pointCount; ++i) {
    const Vec2f &b = points[i-1];
    const Vec2f &c = points[i];

    const Vec2f bc = b.vectorTo(c);
    const Vec2f normalBC = bc.ortho().normalized();
    const Vec2f dBC = (normalBC * lineWidth);
    const Vec2f c0 = c - dBC;
    const Vec2f c1 = c + dBC;

    const Vec2f miter = (ab.normalized() + bc.normalized()).normalized().ortho();
    const Vec2f m = miter * (lineWidth / miter.dotProduct(normalBC));
    const Vec2f b0 = b - m;
    const Vec2f b1 = b + m;

    glVertex2f(a0.x, a0.y);
    glVertex2f(a1.x, a1.y);
    glVertex2f(b1.x, b1.y);

    glVertex2f(b1.x, b1.y);
    glVertex2f(b0.x, b0.y);
    glVertex2f(a0.x, a0.y);

    ab = bc;
    a0 = b0;
    a1 = b1;
  }

  {
    const Vec2f b = points[pointCount-1];

    const Vec2f normalAB = ab.ortho().normalized();
    const Vec2f dAB = (normalAB * lineWidth);
    const Vec2f b0 = b - dAB;
    const Vec2f b1 = b + dAB;

    glVertex2f(a0.x, a0.y);
    glVertex2f(a1.x, a1.y);
    glVertex2f(b1.x, b1.y);

    glVertex2f(b1.x, b1.y);
    glVertex2f(b0.x, b0.y);
    glVertex2f(a0.x, a0.y);
  }

  glEnd();
}

float Track::length() const {
  float len = 0.0f;
  for (usize i = 1; i < pointCount; ++i) {
    const Vec2f &p0 = points[i - 1];
    const Vec2f &p1 = points[i];
    len += p0.distance(p1);
  }
  return len;
}

Vec2f Track::positionAtLength(float l) const {
  ASSERT(!isEmpty());
  for (usize i = 1; i < pointCount; ++i) {
    const Vec2f &p0 = points[i - 1];
    const Vec2f &p1 = points[i];
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
