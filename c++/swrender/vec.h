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

  Vec2 ortho() const { return Vec2(y, x); }

  // Return a vector W, result of U projected on V and clamped to V
  Vec2 projectClamped(const Vec2 v) const {
    const Vec2 axis = v.normalized();
    const float dot = axis.dotProduct(*this);
    return axis * clamp(dot, 0, v.length());
  }

  Vec2 rotateAround(const Vec2 origin, float angle) const {
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

  static float triangleArea(Vec2 a, Vec2 b, Vec2 c) {
    return ((c.x - a.x) * (b.y - a.y) - (b.x - a.x) * (c.y - a.y)) / 2.0f;
  }
};

struct Vec3 {
  float x;
  float y;
  float z;

  Vec3() : x(0), y(0), z(0) {}
  Vec3(float x, float y, float z) : x(x), y(y), z(z) {}

  Vec3 operator*(float s) const { return Vec3(x * s, y * s, z * s); }
  Vec3 operator/(float s) const { return Vec3(x / s, y / s, z / s); }
  Vec3 operator+(const Vec3 o) const { return Vec3(x + o.x, y + o.y, z + o.z); }
  Vec3 operator-(const Vec3 o) const { return Vec3(x - o.x, y - o.y, z - o.z); }

  Vec3 operator+(const Vec2 o) const { return Vec3(x + o.x, y + o.y, z); }

  Vec3 invert() const {
    return Vec3(1.0f / x, 1.0f / y, 1.0f / z);
  }

  Vec3 rotateZ(float angle) const {
    const float cosa = cos(angle);
    const float sina = sin(angle);
    return Vec3(cosa * x - sina * y, sina * x + cosa * y, z);
  }

  Vec3 rotateX(float angle) const {
    const float cosa = cos(angle);
    const float sina = sin(angle);
    return Vec3(x, cosa * y - sina * z, sina * y + cosa * z);
  }

  Vec3 rotateY(float angle) const {
    const float cosa = cos(angle);
    const float sina = sin(angle);
    return Vec3(cosa * x - sina * z, y, sina * x + cosa * z);
  }

  float dotProduct(const Vec3 o) const { return x * o.x + y * o.y + z * o.z; }

  Vec3 crossProduct(const Vec3 o) const {
    return Vec3(y * o.z - z * o.y,
                z * o.x - x * o.z,
                x * o.y - y * o.x);
  }

  float length() const {return sqrt(dotProduct(*this)); }

  Vec3 normalized() const { return *this / length(); }
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

Vec3 barycentricCoordinates2(Vec2 a, Vec2 b, Vec2 c, Vec2 p);
Vec3 barycentricCoordinates3(Vec3 a, Vec3 b, Vec3 c, Vec3 p);

#endif
