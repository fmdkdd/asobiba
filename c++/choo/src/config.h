#ifndef CONFIG_H
#define CONFIG_H

#include "vec.h"

namespace config {

static const Vec4f backgroundColor =
    Vec4f(77.0f / 255.0f, 74.0f / 255.0f, 90.0f / 255.0f, 0.f);

static const float previewPointRadius = 10.0f;
static const u32 previewPointResolution = 32;
static const float previewLineWidth = 5.0f;

static const Vec4f trackColor(1.0f, 1.0f, 0.0f, 1.0f);
static const float trackPointRadius = 10.0f;
static const u32 trackPointResolution = 32;
static const u32 trackLineWidth = 5.0f;

}; // namespace config

#endif
