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

void Train::setPath(Vec2i *points, u32 pointCount) {
  for (u32 i=0; i < pointCount; ++i)
    pathPoints[i] = points[i];
  pathPointCount = pointCount;

  hasPath = true;
}

void Train::update() {
  if (!hasPath)
    return;

  pathPosition += speed;

  if (pathPosition >= pathLength()) {
    pathPosition = 0;
  }
}

static const float RADIAN_TO_DEGREES = 57.2958279f;

void Train::render() {
  if (!hasPath)
    return;

  const float car_width = 0.1f;
  const float car_height = 0.05f;
  const float car_space = 0.1f;

  for (u32 i = 0; i < carCount; ++i) {
    float carCenter = pathPosition - i * (0.01f + car_space);
    Vec2f carFront = interpolatePathPosition(carCenter + 0.01f);
    Vec2f carTail = interpolatePathPosition(carCenter - 0.01f);
    Vec2f carVec = carFront.vectorTo(carTail);
    direction = atan(carVec.y / carVec.x) * RADIAN_TO_DEGREES;

    Vec2f center = interpolatePathPosition(carCenter);
    float x = center.x;
    float y = center.y;

    // float x = pos - 0.5;
    // float y = 0.5 * sin(pos * PI * 3);
    // float h = 0.001f;
    // float y2 = 0.5 * sin((pos + h) * PI * 3);

    // direction = ((y2 - y) / h) * RADIAN_TO_DEGREES;

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

    // car_x -= car_width + car_space;
    // car_y -=
  }
}

u32 Train::pathLength() const {
  u32 length = 0;

  for (u32 i=1; i < pathPointCount; ++i) {
    const Vec2i &a = pathPoints[i - 1];
    const Vec2i &b = pathPoints[i];
    length += a.distance(b);
  }

  return length;
}

Vec2f Train::interpolatePathPosition(u32 t) const {

  return Vec2f();
}
