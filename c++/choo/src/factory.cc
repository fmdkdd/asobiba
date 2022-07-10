#include "factory.h"

#include <glad/glad.h>

void Factory::init(u32 id) {
  this->id = id;

  pos = Vec2f(0.0f, 0.0f);
  size = 1;

  for (usize i = 0; i < MAX_INPUTS; ++i)
    inputCargo[i].type = NONE;
  for (usize i = 0; i < MAX_OUTPUTS; ++i)
    outputCargo[i].type = NONE;
}

bool Factory::hasInputs(const ProductionRule& rule) const {
  for (usize i=0; i < ProductionRule::MAX_INPUTS; ++i) {
    CargoType inputType = rule.inputTypes[i];

    if (inputType == NONE)
      break;

    ASSERT(inputCargo[i].type == inputType);
    if (inputCargo[i].count < rule.inputCount[i])
      return false;
  }
  return true;
}

void Factory::consumeInputs(const ProductionRule& rule) {
  for (usize i=0; i < ProductionRule::MAX_INPUTS; ++i) {
    CargoType inputType = rule.inputTypes[i];

    if (inputType == NONE)
      break;

    ASSERT(inputCargo[i].type == inputType);
    ASSERT(inputCargo[i].count >= rule.inputCount[i]);
    inputCargo[i].count -= rule.inputCount[i];
  }
}

void Factory::update() {
  for (usize i = 0; i < MAX_OUTPUTS; ++i) {
    Cargo &c = outputCargo[i];

    if (c.type == NONE)
      break;

    const ProductionRule& rule = *c.productionRule;

    if (hasInputs(rule)) {
      c.generateCounter += rule.generateRate;
      if (c.generateCounter > rule.generateCounterTarget) {
        c.generateCounter -= rule.generateCounterTarget;

        u32 newCount = c.count + rule.outputCount;
        if (newCount < c.capacity) {
          consumeInputs(rule);
          c.count = newCount;
        }
      }
    }
  }
}

void Factory::render() const {
  static const float sizeFactor = 100.0f;
  const float width = size * sizeFactor;
  const float height = size * sizeFactor;

  glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(pos.x, pos.y, 0.0f);
  glRectf(-width / 2.0f, -height / 2.0f, width / 2.0f, height / 2.0f);

  // Cargo fill bar atop
  if (outputCargo[0].type != NONE) {
    static const float cargoBarHeight = 15.0f;
    static const float cargoBarSpacing = 5.0f;
    float cargoFill = (float)outputCargo[0].count / outputCargo[0].capacity;
    glRectf(-width / 2.0f, height / 2.0f + cargoBarHeight + cargoBarSpacing,
            -width / 2.0f + (cargoFill * width), height / 2.0f + cargoBarSpacing);
  }
}
