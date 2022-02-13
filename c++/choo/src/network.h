#ifndef NETWORK_H
#define NETWORK_H

#include "utils.h"
#include "vec.h"

typedef u32 PointId;

struct Network;

struct Path {
  static const usize MAX_POINTS = 256;

  PointId points[MAX_POINTS];
  u32 pointCount;

  const Network *network;

  u32 length() const;

  Vec2i pointCoords(usize i) const;
};

struct Network {
  struct Edge {
    u32 from;
    u32 to;
  };

  static const usize MAX_POINTS = 1024;
  static const usize MAX_EDGES = 4096;

  Vec2i points[MAX_POINTS];
  usize pointCount;

  Edge edges[MAX_EDGES];
  usize edgeCount;

  void init();
  void render() const;

  PointId addPoint(Vec2i p);
  void addEdge(PointId from, PointId to);

  Vec2i getPoint(PointId id) const;
  Optional<PointId> getClosestPoint(Vec2i p, float maxDistance) const;

  void getShortestPath(PointId from, PointId to, Path *path) const;
};

inline Vec2i Network::getPoint(PointId id) const {
  ASSERT(id < MAX_POINTS);
  return points[id];
}

inline Vec2i Path::pointCoords(usize i) const {
  ASSERT(network != nullptr);
  return network->getPoint(points[i]);
}

#endif
