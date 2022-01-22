#ifndef TRAIN_H
#define TRAIN_H

#include "track.h"
#include "vec.h"

struct Train {
  usize id;

  float pos;
  int carCount;
  float speed;
  float direction;

  Track *track;

  void init(u32 id);
  void update();
  void render();

  Vec2 trackPos(float t) const { return track->positionAtLength(t); }
};

#endif
