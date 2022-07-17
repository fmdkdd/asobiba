#ifndef GFX_H
#define GFX_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>

#include "vec.h"

struct Controls;

struct ColorRGBA {
  u8 r;
  u8 g;
  u8 b;
  u8 a;
};

struct Font {
  GLuint texture;
  int charWidth;
  int charHeight;
  int texWidth;
  int texHeight;
  u8 charIndex[256];

  void init(const char *file, int width, int height, const char *mapping,
            usize charMappingLength);
  void quit();

  void drawCharAt(unsigned char c, int x, int y);
};

struct Gfx {
  const Controls *controls;
  Font font;

  void init(const Controls& controls);
  void quit();

  u32 text(const char *s, int x, int y);
  SDL_Rect boxedText(const char *s, u32 x, u32 y);
  bool button(const char *s, u32 x, u32 y);
};

void clearScreen(ColorRGBA c);
void setDrawColor(ColorRGBA c);
void drawRect(int x, int y, int w, int h);
void fillRect(float x, float y, float w, float h);
void drawCircle(Vec2f center, float radius, u32 pointCount);
void drawLine(Vec2f *points, usize pointCount, float width);

#endif
