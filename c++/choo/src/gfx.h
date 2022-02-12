#ifndef GFX_H
#define GFX_H

#include "vec.h"

void drawCircle(Vec2f center, float radius, u32 pointCount);
void drawLine(Vec2f *points, usize pointCount, float width);

#endif
