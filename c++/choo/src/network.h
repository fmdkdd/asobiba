#ifndef NETWORK_H
#define NETWORK_H

#include "utils.h"
#include "vec.h"

typedef u32 PointId;

struct Network {
  struct Point {
    s32 x;
    s32 y;
  };

  struct Edge {
    u32 from;
    u32 to;
  };

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

  Point getPoint(PointId id) const;
  Optional<PointId> getClosestPoint(Vec2i p, float maxDistance) const;
};

inline Network::Point Network::getPoint(PointId id) const { ASSERT(id < MAX_POINTS); return points[id]; }

#endif
