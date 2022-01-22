#ifndef VEC_H
#define VEC_H

#include <cmath>

#include "utils.h"

static const float PI = 3.1415926535897932384626433f;

struct Vec2 {
  float x;
  float y;

  Vec2() : x(0), y(0) {}
  Vec2(float x, float y) : x(x), y(y) {}
  ~Vec2() {}

  Vec2 operator+(const Vec2 o) const { return Vec2(x + o.x, y + o.y); }
  Vec2 operator-(const Vec2 o) const { return Vec2(x - o.x, y - o.y); }
  Vec2 operator*(float s) const { return Vec2(x * s, y * s); }
  Vec2 operator/(float s) const { return Vec2(x / s, y / s); }
  bool operator==(const Vec2 o) const { return x == o.x && y == o.y; }

  float distance(const Vec2 o) const {
    const float dx = o.x - x;
    const float dy = o.y - y;
    return sqrt(dx * dx + dy * dy);
  }

  float dotProduct(const Vec2 o) const { return x * o.x + y * o.y; }

  Vec2 vectorTo(const Vec2 o) const { return Vec2(o.x - x, o.y - y); }

  float length() const { return distance(Vec2()); }

  float angle() const { return atan2(y, x); }

  Vec2 normalized() const { return *this / length(); }

  Vec2 ortho() const { return Vec2(-y, x); }

  // Return a vector W, result of U projected on V and clamped to V
  Vec2 projectClamped(const Vec2 v) const {
    const Vec2 axis = v.normalized();
    const float dot = axis.dotProduct(*this);
    return axis * clamp(dot, 0, v.length());
  }

  Vec2 rotateAround(const Vec2 origin, float angle) {
    const float cosa = cos(angle);
    const float sina = sin(angle);
    return Vec2((cosa * (x - origin.x) - sina * (y - origin.y)) + origin.x,
                (sina * (x - origin.x) + cosa * (y - origin.y)) + origin.y);
  }

  static Vec2 lerp(const Vec2 a, const Vec2 b, float t) {
    return a * t + b * (1 - t);
  }

  static float lerp(float a, float b, float t) { return a * t + b * (1 - t); }

  static float clamp(float x, float a, float b) {
    if (x < a)
      return a;
    if (x > b)
      return b;
    return x;
  }
};

struct Vec4 {
  float x;
  float y;
  float z;
  float w;

  Vec4() : x(0), y(0), z(0), w(0) {}
  Vec4(float x, float y, float z, float w) : x(x), y(y), z(z), w(w) {}

  Vec4 operator*(float s) const { return Vec4(x * s, y * s, z * s, w * s); }
};

#endif
