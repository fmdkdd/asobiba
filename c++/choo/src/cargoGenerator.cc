#include "cargoGenerator.h"

#include <glad/glad.h>

void CargoGenerator::init(u32 id) {
  this->id = id;

  pos = Vec2f(0.0f, 0.0f);
  cargoCount = 100;
  size = 1;
}

void CargoGenerator::update() {
}

void CargoGenerator::render() const {
  static const float sizeFactor = 0.1f;
  const float width  = size * sizeFactor;
  const float height = size * sizeFactor;

  glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
  glLineWidth(2.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(pos.x, pos.y, 0.0f);
  glRectf(0, 0, width, height);
}
