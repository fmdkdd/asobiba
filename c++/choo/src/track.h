#ifndef TRACK_H
#define TRACK_H

#include "vec.h"

struct Track {
  static const int MAX_POINTS = 256;

  Vec2 points[MAX_POINTS];
  usize pointCount;

  void init();
  void add(float x, float y);

  void render() const;
  float length() const;
  Vec2 positionAtLength(float l) const;

  bool isEmpty() const { return length() == 0.0f; }
};

#endif
