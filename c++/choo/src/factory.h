#ifndef CARGO_GENERATOR_H
#define CARGO_GENERATOR_H

#include "utils.h"
#include "vec.h"

enum CargoType {
  NONE,
  COAL,
  IRON,
  GOLD,
};

static const char *CARGO_TYPE_NAME[] = {"None", "Coal", "Iron", "Gold"};

struct ProductionRule {
  static const usize MAX_INPUTS = 8;
  CargoType inputTypes[MAX_INPUTS];
  u32 inputCount[MAX_INPUTS];
  u32 outputCount;

  u32 generateRate;
  u32 generateCounterTarget;
};

struct Cargo {
  CargoType type;
  u32 count;
  u32 capacity;

  // For output cargo
  const ProductionRule *productionRule;
  u32 generateCounter;
};

static const ProductionRule defaultProductionRules[] = {
  {{NONE}, {0}, 1, 1, 100},
  {{COAL}, {10}, 1, 1, 100},
};

enum DefaultProductionRulesNames {
  COAL_MINE,
  IRON_PLANT,
};

struct Factory {
  usize id;

  Vec2f pos;
  u32 size;

  static const usize MAX_INPUTS = 8;
  static const usize MAX_OUTPUTS = 8;

  Cargo inputCargo[MAX_INPUTS];
  Cargo outputCargo[MAX_OUTPUTS];

  void init(u32 id);
  void update();
  void render() const;

  bool hasInputs(const ProductionRule &rule) const;
  void consumeInputs(const ProductionRule &rule);
};

#endif
