#ifndef TRACK_H
#define TRACK_H

#include "utils.h"
#include "vec.h"

struct Game;

struct Track {
  u32 id;

  static const int MAX_POINTS = 256;

  Vec2 points[MAX_POINTS];
  usize pointCount;

  void init(u32 id);
  void add(float x, float y);

  void update();
  void render() const;
  float length() const;
  Vec2 positionAtLength(float l) const;

  bool isEmpty() const { return length() == 0.0f; }
};

#endif
