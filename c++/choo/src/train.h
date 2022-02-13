#ifndef TRAIN_H
#define TRAIN_H

#include "track.h"
#include "vec.h"
#include "network.h"

struct Train {
  usize id;

  u32 pathPosition;
  u32 carCount;
  s32 speed;
  s32 direction;

  bool hasPath;

  static const usize MAX_POINTS = 256;
  Vec2i pathPoints[MAX_POINTS];
  u32 pathPointCount;

  void init(u32 id);
  void update();
  void render();

  void setPath(Vec2i *points, u32 pointCount);

  u32 pathLength() const;
  Vec2f interpolatePathPosition(u32 t) const;
};

#endif
