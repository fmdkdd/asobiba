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

struct Texture {
  GLuint textureId;
  int width;
  int height;

  void init(SDL_Surface *surface);
  void quit();
};

struct Font {
  Texture texture;
  int charWidth;
  int charHeight;
  u8 charIndex[256];

  void init(const char *file, int width, int height, const char *mapping,
            usize charMappingLength);
  void quit();

  void drawCharAt(unsigned char c, int x, int y);
};

struct AnimatedSprite {
  Texture *spritesheet;
  Recti *spriteRects;
  usize spriteRectsCount;

  u32 animationStep;
  u32 animationSpeed;
  u32 animationStepCounter;

  void drawAt(u32 x, u32 y);
  void step();
};

struct Gfx {
  const Controls *controls;
  Font font;

  void init(const Controls& controls);
  void quit();

  u32 text(const char *s, int x, int y);
  SDL_Rect boxedText(const char *s, u32 x, u32 y);
  bool button(const char *s, u32 x, u32 y);

  void loadImage(const char* path, Texture* texture);
};

void clearScreen(ColorRGBA c);
void setDrawColor(ColorRGBA c);
void drawRect(int x, int y, int w, int h);
void fillRect(float x, float y, float w, float h);
void drawCircle(Vec2f center, float radius, u32 pointCount);
void drawLine(Vec2f *points, usize pointCount, float width);

#endif
