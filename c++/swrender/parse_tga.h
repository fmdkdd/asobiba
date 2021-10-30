#ifndef PARSE_TGA
#define PARSE_TGA

#include "utils.h"

struct TGA {
  u16 width;
  u16 height;
  u8 depth;
  u32* rgbaData;
};

void parseTGA(const u8* raw, u32 rawSize, TGA *tga);
void parseTGAFile(const char *filename, TGA *tga);

#endif
