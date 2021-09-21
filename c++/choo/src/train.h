#ifndef TRAIN_H
#define TRAIN_H

#include "track.h"
#include "vec.h"

struct Train {
  float pos;
  int carCount;
  float speed;
  float direction;

  Track *track;

  void init();
  void step();

  Vec2 trackPos(float t) const {
    return track->positionAtLength(t);
  }

  void render();
};

#endif
