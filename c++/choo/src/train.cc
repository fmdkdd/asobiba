#include <glad/glad.h>

#include "train.h"

void Train::init(u32 id) {
  this->id = id;
  pathPosition = 0;
  carCount = 4;
  speed = 1;
  direction = 0;

  hasPath = false;
}

void Train::setPath(Path &path) {
  this->path = path;
  hasPath = true;
}

void Train::update() {
  if (!hasPath)
    return;

  pathPosition += speed;

  if (pathPosition >= path.length()) {
    pathPosition = 0;
  }
}

static const float RADIAN_TO_DEGREES = 57.2958279f;

void Train::render() {
  if (!hasPath)
    return;

  const float car_width = 30.0f;
  const float car_height = 20.0f;
  const float car_space = 40.0f;

  for (u32 i = 0; i < carCount; ++i) {
    float carCenter = pathPosition - i * (1.0f + car_space);
    Vec2f carFront = interpolatePathPosition(carCenter + 1.0f);
    Vec2f carTail = interpolatePathPosition(carCenter - 1.0f);
    Vec2f carVec = carFront.vectorTo(carTail);
    direction = atan(carVec.y / carVec.x) * RADIAN_TO_DEGREES;

    Vec2f center = interpolatePathPosition(carCenter);
    float x = center.x;
    float y = center.y;

    glColor4f(1.0f, 0.0f, 0.0f, 1.0f);
    glLineWidth(2.0f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(x, y, 0.0f);
    glRotatef(direction, 0.0f, 0.0f, 1.0f);
    glTranslatef(-car_width / 2, -car_height / 2, 0.0f);
    glRectf(0, 0, car_width, car_height);
    glBegin(GL_LINES);
    glVertex2f(0, car_height / 2);
    glVertex2f(1.5 * car_width, car_height / 2);
    glEnd();
  }
}

Vec2f Train::interpolatePathPosition(u32 t) const {
  ASSERT(path.pointCount > 0);

  float l = t;

  for (usize i = 1; i < path.pointCount; ++i) {
    const Vec2f &p0 = path.pointCoords(i - 1);
    const Vec2f &p1 = path.pointCoords(i);
    const float d = p0.distance(p1);
    if (l > d)
      l -= d;
    else {
      const float remainingRatio = l / d;
      return p0 + (p0.vectorTo(p1) * remainingRatio);
    }
  }
  return path.pointCoords(path.pointCount - 1);
}
