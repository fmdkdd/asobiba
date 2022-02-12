#ifndef NETWORK_H
#define NETWORK_H

#include "utils.h"
#include "vec.h"

struct Network {
  struct Point {
    s32 x;
    s32 y;
  };

  struct Edge {
    u32 from;
    u32 to;
  };

  typedef u32 PointId;

  static const usize MAX_POINTS = 1024;
  static const usize MAX_EDGES = 4096;

  Point points[MAX_POINTS];
  usize pointCount;

  Edge edges[MAX_EDGES];
  usize edgeCount;

  void init();
  void render() const;

  PointId addPoint(Point p);
  void addEdge(PointId from, PointId to);
};

#endif
