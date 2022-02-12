#ifndef CARGO_GENERATOR_H
#define CARGO_GENERATOR_H

#include "utils.h"
#include "vec.h"

struct Cargo {
  enum Type {
    COAL,
    IRON,
    GOLD,
  };
  Type type;
  u32 amount;
  u32 price;
};

static const char *CARGO_TYPE_NAME[] = {"Coal", "Iron", "Gold"};

struct CargoGenerator {
  static const usize MAX_CARGO_AT_STATION = 256;
  usize id;

  Vec2f pos;
  u32 size;

  Cargo cargo[MAX_CARGO_AT_STATION];
  u32 cargoCount;

  void init(u32 id);
  void update();
  void render() const;
};

#endif
