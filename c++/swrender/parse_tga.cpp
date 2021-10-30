#include "parse_tga.h"

#include <cstdio>
#include <cstdlib>

static u32 readu32le(const u8 **p) {
  ENSURE(p != nullptr);

  u32 x = ((*p)[3] << 24) | ((*p)[2] << 16) | ((*p)[1] << 8) | (*p)[0];
  *p += 4;
  return x;
}

static u16 readu16le(const u8 **p) {
  ENSURE(p != nullptr);

  u16 x = ((*p)[1] << 8) | (*p)[0];
  *p += 2;
  return x;
}

static u8 readu8(const u8 **p) {
  ENSURE(p != nullptr);

  u8 x = (*p)[0];
  *p += 1;
  return x;
}

static bool atFileEnd(FILE *f) {
  ENSURE(f != nullptr);

  u8 dumb;
  size_t readSize = fread(&dumb, sizeof(dumb), 1, f);
  if (readSize == 0) {
    ENSURE(feof(f));
    return true;
  } else
    return false;
}

void parseTGA(const u8 *raw, u32 rawSize, TGA *tga) {
  ENSURE(raw != nullptr);
  ENSURE(tga != nullptr);

  const u8 *r = raw;

  u8 idLength = readu8(&r);
  u8 colorMapType = readu8(&r);
  u8 imageType = readu8(&r);
  u16 colorMapFirstEntryIndex = readu16le(&r);
  u16 colorMapLength = readu16le(&r);
  u8 colorBitsPerPixel = readu8(&r);

  ASSERT(idLength == 0);
  ASSERT(colorMapType == 0);
  ASSERT(imageType == 10);

  UNUSED(colorMapFirstEntryIndex);
  UNUSED(colorMapLength);
  UNUSED(colorBitsPerPixel);

  u16 xOrigin = readu16le(&r);
  u16 yOrigin = readu16le(&r);
  tga->width = readu16le(&r);
  tga->height = readu16le(&r);
  tga->depth = readu8(&r);
  u8 descriptor = readu8(&r);

  ASSERT(xOrigin == 0);
  ASSERT(yOrigin == 0);
  ASSERT(descriptor == 0);

  printf("width: %d\n", tga->width);
  printf("height: %d\n", tga->height);
  printf("depth: %d\n", tga->depth);

  u64 pixelCount = tga->width * tga->height;
  tga->rgbaData = (u32 *)malloc(tga->width * tga->height * sizeof(u32));
  ENSURE(tga->rgbaData != nullptr);

  u64 pixelIndex = 0;
  while (pixelIndex < pixelCount) {
    u8 packet = readu8(&r);
    bool isRunLength = packet & 0x80;
    u8 count = (packet & 0x7f) + 1;

    if (isRunLength) {
      u8 blue = readu8(&r);
      u8 green = readu8(&r);
      u8 red = readu8(&r);
      u32 rgba = (red << 24) | (green << 16) | (blue << 8);

      for (u8 i = 0; i < count; ++i) {
        tga->rgbaData[pixelIndex++] = rgba;
      }
    } else {
      for (u8 i = 0; i < count; ++i) {
        u8 blue = readu8(&r);
        u8 green = readu8(&r);
        u8 red = readu8(&r);
        u32 rgba = (red << 24) | (green << 16) | (blue << 8);
        tga->rgbaData[pixelIndex++] = rgba;
      }
    }
  }

  ASSERT((r - raw) <= rawSize);
}

void destroyTGA(TGA *tga) {
  ASSERT(tga != nullptr);
  ASSERT(tga->rgbaData != nullptr);

  free(tga->rgbaData);
  tga->rgbaData = nullptr;
}

void parseTGAFile(const char *filename, TGA *tga) {
  ENSURE(filename != nullptr);

  FILE *f = fopen(filename, "rb");
  ENSURE(f != nullptr);

  int ret = fseek(f, 0, SEEK_END);
  ENSURE(ret == 0);
  long fileSize = ftell(f);
  ENSURE(fileSize > 0);

  u8 *raw = (u8 *)malloc(fileSize);
  ENSURE(raw != nullptr);

  ret = fseek(f, 0, SEEK_SET);
  ENSURE(ret == 0);
  size_t readSize = fread(raw, sizeof(u8), fileSize, f);
  ENSURE(readSize == (size_t)fileSize);

  ret = fclose(f);
  ENSURE(ret == 0);

  parseTGA(raw, readSize, tga);

  free(raw);
}
