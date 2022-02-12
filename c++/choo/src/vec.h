#ifndef VEC_H
#define VEC_H

#include <cmath>

#include "utils.h"

static const float PI = 3.1415926535897932384626433f;

struct Vec2i {
  int x;
  int y;

  Vec2i() : x(0), y(0) {}
  Vec2i(int x, int y) : x(x), y(y) {}
  ~Vec2i() {}

  Vec2i operator+(const Vec2i o) const { return Vec2i(x + o.x, y + o.y); }
  Vec2i operator-(const Vec2i o) const { return Vec2i(x - o.x, y - o.y); }

  bool operator==(const Vec2i o) const { return x == o.x && y == o.y; }
};

struct Vec2f {
  float x;
  float y;

  Vec2f() : x(0), y(0) {}
  Vec2f(float x, float y) : x(x), y(y) {}
  ~Vec2f() {}

  Vec2f operator+(const Vec2f o) const { return Vec2f(x + o.x, y + o.y); }
  Vec2f operator-(const Vec2f o) const { return Vec2f(x - o.x, y - o.y); }
  Vec2f operator*(float s) const { return Vec2f(x * s, y * s); }
  Vec2f operator/(float s) const { return Vec2f(x / s, y / s); }
  bool operator==(const Vec2f o) const { return x == o.x && y == o.y; }

  float distance(const Vec2f o) const {
    const float dx = o.x - x;
    const float dy = o.y - y;
    return sqrt(dx * dx + dy * dy);
  }

  float dotProduct(const Vec2f o) const { return x * o.x + y * o.y; }

  Vec2f vectorTo(const Vec2f o) const { return Vec2f(o.x - x, o.y - y); }

  float length() const { return distance(Vec2f()); }

  float angle() const { return atan2(y, x); }

  Vec2f normalized() const { return *this / length(); }

  Vec2f ortho() const { return Vec2f(-y, x); }

  // Return a vector W, result of U projected on V and clamped to V
  Vec2f projectClamped(const Vec2f v) const {
    const Vec2f axis = v.normalized();
    const float dot = axis.dotProduct(*this);
    return axis * clamp(dot, 0, v.length());
  }

  Vec2f rotateAround(const Vec2f origin, float angle) {
    const float cosa = cos(angle);
    const float sina = sin(angle);
    return Vec2f((cosa * (x - origin.x) - sina * (y - origin.y)) + origin.x,
                (sina * (x - origin.x) + cosa * (y - origin.y)) + origin.y);
  }

  static Vec2f lerp(const Vec2f a, const Vec2f b, float t) {
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

struct Vec4f {
  float x;
  float y;
  float z;
  float w;

  Vec4f() : x(0), y(0), z(0), w(0) {}
  Vec4f(float x, float y, float z, float w) : x(x), y(y), z(z), w(w) {}

  Vec4f operator*(float s) const { return Vec4f(x * s, y * s, z * s, w * s); }
};

#endif
