#include "train.h"

#include <glad/glad.h>

void Train::init() {
  pos = 0;
  carCount = 3;
  speed = 0.001f;
  direction = 45.0f;
  track = nullptr;
}

void Train::step() {
  if (track == nullptr || track->isEmpty())
    return;

  pos += speed;

  float trackLength = track->length();
  if (pos >= trackLength) {
    pos = 0.0f;
  }
}

static const float RADIAN_TO_DEGREES = 57.2958279f;

void Train::render() {
  if (track == nullptr || track->isEmpty())
    return;

  const float car_width = 0.1f;
  const float car_height = 0.05f;
  const float car_space = 0.1f;

  for (int i = 0; i < carCount; ++i) {
    float carCenter = pos - i * (0.01f + car_space);
    Vec2 carFront = trackPos(carCenter + 0.01f);
    Vec2 carTail = trackPos(carCenter - 0.01f);
    Vec2 carVec = carFront.vectorTo(carTail);
    direction = atan(carVec.y / carVec.x) * RADIAN_TO_DEGREES;

    Vec2 center = trackPos(carCenter);
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
