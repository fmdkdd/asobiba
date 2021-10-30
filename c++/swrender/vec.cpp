#include "vec.h"

Vec3 barycentricCoordinates2(Vec2 a, Vec2 b, Vec2 c, Vec2 p) {
  float area  = Vec2::triangleArea(a, b, c);
  float beta  = Vec2::triangleArea(c, a, p);
  float gamma = Vec2::triangleArea(a, b, p);

  float v = beta / area;
  float w = gamma / area;
  float u = 1.0f - v - w;

  return Vec3(u, v, w);
}

Vec3 barycentricCoordinates3(Vec3 a, Vec3 b, Vec3 c, Vec3 p) {
  Vec3 n = (b-a).crossProduct(c-a);
  Vec3 na = (c-b).crossProduct(p-b);
  Vec3 nb = (a-c).crossProduct(p-c);
  Vec3 nc = (b-a).crossProduct(p-a);

  float l = n.dotProduct(n);

  float u = n.dotProduct(na) / l;
  float v = n.dotProduct(nb) / l;
  float w = n.dotProduct(nc) / l;

  return Vec3(u, v, w);
}
