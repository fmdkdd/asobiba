#include <glad/glad.h>

#include "gfx.h"
#include "network.h"
#include "vec.h"

void drawCircle(Vec2f center, float radius, u32 pointCount) {
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(center.x, center.y);

  float angle = 0;
  float t = 2 * PI / ((float)(pointCount - 1));

  for (u32 i = 0; i < pointCount; ++i) {
    Vec2f p = center + Vec2f(cos(angle), sin(angle)) * radius;
    glVertex2f(p.x, p.y);
    angle += t;
  }
  glEnd();
}

void drawLine(Vec2f *points, usize pointCount, float width) {
  ASSERT(pointCount > 1);

  float lineWidth = width;
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
    const Vec2f &b = points[i - 1];
    const Vec2f &c = points[i];

    const Vec2f bc = b.vectorTo(c);
    const Vec2f normalBC = bc.ortho().normalized();
    const Vec2f dBC = (normalBC * lineWidth);
    const Vec2f c0 = c - dBC;
    const Vec2f c1 = c + dBC;

    const Vec2f miter =
        (ab.normalized() + bc.normalized()).normalized().ortho();
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
    const Vec2f b = points[pointCount - 1];

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

void drawPath(const Network *network, const Path &path, float width) {
  ASSERT(path.pointCount > 1);

  float lineWidth = width;
  glBegin(GL_TRIANGLES);

  Vec2f ab;
  Vec2f a0;
  Vec2f a1;

  {
    const Vec2f a = network->getPoint(path.points[0]);
    const Vec2f b = network->getPoint(path.points[1]);

    ab = a.vectorTo(b);
    const Vec2f normalAB = ab.ortho().normalized();
    const Vec2f dAB = (normalAB * lineWidth);
    a0 = a - dAB;
    a1 = a + dAB;
  }

  for (usize i = 2; i < path.pointCount; ++i) {
    const Vec2f &b = network->getPoint(path.points[i - 1]);
    const Vec2f &c = network->getPoint(path.points[i]);

    const Vec2f bc = b.vectorTo(c);
    const Vec2f normalBC = bc.ortho().normalized();
    const Vec2f dBC = (normalBC * lineWidth);
    const Vec2f c0 = c - dBC;
    const Vec2f c1 = c + dBC;

    const Vec2f miter =
        (ab.normalized() + bc.normalized()).normalized().ortho();
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
    const Vec2f b = network->getPoint(path.points[path.pointCount - 1]);

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
