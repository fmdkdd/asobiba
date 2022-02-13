#ifndef GFX_H
#define GFX_H

#include "vec.h"

struct Network;
struct Path;

void drawCircle(Vec2f center, float radius, u32 pointCount);
void drawLine(Vec2f *points, usize pointCount, float width);
void drawPath(const Network *network, const Path &path, float width);

#endif
