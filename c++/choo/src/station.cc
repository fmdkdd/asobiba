#include "station.h"

#include <cmath>
#include <glad/glad.h>

void Station::init(u32 id) {
  this->id = id;

  pos = Vec2f(0.0f, 0.0f);
}

void Station::update() {}

void Station::render() const {
  static const float radius = 0.1f;
  static const float thickness = 1.0f;
  static const usize segments = 24;
  static const float angleInc = PI * 2.0f / (float)segments;

  glColor4f(0.0f, 1.0f, 1.0f, 1.0f);
  glLineWidth(thickness);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(pos.x, pos.y, 0.0f);
  glBegin(GL_LINE_LOOP);
  for (usize i = 0; i < segments; ++i) {
    float angle = i * angleInc;
    glVertex2f(cos(angle) * radius, sin(angle) * radius);
  }
  glEnd();
}
