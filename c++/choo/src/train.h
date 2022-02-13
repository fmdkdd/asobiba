#ifndef TRAIN_H
#define TRAIN_H

#include "vec.h"
#include "network.h"

struct Train {
  usize id;

  u32 pathPosition;
  u32 carCount;
  s32 speed;
  s32 direction;

  bool hasPath;
  Path path;

  void init(u32 id);
  void update();
  void render();

  void setPath(Path &path);

  Vec2f interpolatePathPosition(u32 t) const;
};

#endif
