#ifndef STATION_H
#define STATION_H

#include "utils.h"
#include "vec.h"

struct Station {
  usize id;

  Vec2f pos;

  void init(u32 id);
  void update();
  void render() const;
};

#endif
