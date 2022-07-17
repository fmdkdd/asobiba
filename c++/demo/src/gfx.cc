#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_opengl.h>

#include "gfx.h"

#include "controls.h"
#include "vec.h"

void Texture::init(SDL_Surface *surface) {
  glGenTextures(1, &textureId);
  glBindTexture(GL_TEXTURE_2D, textureId);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, surface->pixels);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  glBindTexture(GL_TEXTURE_2D, 0);

  width = surface->w;
  height = surface->h;
}

void Texture::quit() {
  glDeleteTextures(1, &textureId);
}

static const char *FONT_CHARS =
    " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',."
    "/~!@#$%^&*()_+{}|:\"<>?";

void Font::init(const char *file, int charWidth, int charHeight,
                const char *charMapping, usize charMappingLength) {
  texture.init(SDL_LoadBMP(file));

  this->charHeight = charHeight;
  this->charWidth = charWidth;

  memset(charIndex, 0, sizeof(charIndex));
  for (usize i = 0; i < charMappingLength; ++i) {
    unsigned char c = charMapping[i];
    charIndex[c] = (u8)i;
  }
}

void Font::quit() { texture.quit(); }

void Font::drawCharAt(unsigned char c, int x, int y) {
  int i = charIndex[c];

  SDL_Rect src = {i * charWidth, 0, charWidth, charHeight};

  float tw = (float)texture.width;
  float th = (float)texture.height;

  float u0 = src.x / tw;
  float u1 = (src.x + src.w) / tw;
  float v0 = 0;
  float v1 = src.h / th;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, texture.textureId);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(x, y, 0.0f);
  glScalef(charWidth, charHeight, 0.0f);

  glBegin(GL_QUADS);
  glTexCoord2f(u0, v0);
  glVertex2f(0.0f, 1.0f);
  glTexCoord2f(u1, v0);
  glVertex2f(1.0f, 1.0f);
  glTexCoord2f(u1, v1);
  glVertex2f(1.0f, 0.0f);
  glTexCoord2f(u0, v1);
  glVertex2f(0.0f, 0.0f);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
}

void AnimatedSprite::drawAt(u32 x, u32 y) {
  ASSERT(animationStep < spriteRectsCount);

  Recti& src = spriteRects[animationStep];

  float tw = (float)spritesheet->width;
  float th = (float)spritesheet->height;

  float u0 = src.x / tw;
  float u1 = (src.x + src.w) / tw;
  float v0 = src.y / tw;
  float v1 = (src.y + src.h) / th;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, spritesheet->textureId);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(x, y, 0.0f);
  glScalef(40.0f, 40.0f, 0.0f);

  glBegin(GL_QUADS);
  glTexCoord2f(u0, v0);
  glVertex2f(0.0f, 1.0f);
  glTexCoord2f(u1, v0);
  glVertex2f(1.0f, 1.0f);
  glTexCoord2f(u1, v1);
  glVertex2f(1.0f, 0.0f);
  glTexCoord2f(u0, v1);
  glVertex2f(0.0f, 0.0f);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
}

void AnimatedSprite::step() {
  animationStepCounter += animationSpeed;
  while (animationStepCounter >= 1000) {
    animationStepCounter -= 1000;
    animationStep++;
    if (animationStep >= spriteRectsCount)
      animationStep = 0;
  }
}

void Gfx::init(const Controls &controls) {
  this->controls = &controls;

  font.init("data/font.bmp", 9, 16, FONT_CHARS, strlen(FONT_CHARS));
}

void Gfx::quit() { font.quit(); }

u32 Gfx::text(const char *s, int x, int y) {
  const u32 charWidth = font.charWidth - 1;
  u32 pos = x;

  for (usize i = 0; s[i] != '\0'; ++i) {
    font.drawCharAt(s[i], pos, y);
    pos += charWidth;
  }

  return pos - x + 1;
}

SDL_Rect Gfx::boxedText(const char *s, u32 x, u32 y) {
  const u32 textWidth = text(s, x, y);

  const u32 textHeight = font.charHeight;
  const u32 leftMargin = 2;
  const u32 topMargin = 2;
  const u32 rightMargin = 2;
  const u32 bottomMargin = 2;
  const int box_x = x - leftMargin;
  const int box_y = y - topMargin;
  const int box_w = leftMargin + textWidth + rightMargin;
  const int box_h = topMargin + textHeight + bottomMargin;
  drawRect(box_x, box_y, box_w, box_h);

  return SDL_Rect{box_x, box_y, box_w, box_h};
}

bool Gfx::button(const char *s, u32 x, u32 y) {
  SDL_Rect box = boxedText(s, x, y);

  bool clicked = controls->isMouseButtonHeld(SDL_BUTTON_LEFT) &&
                 SDL_PointInRect(&controls->lastLogicalMousePosition, &box);

  return clicked;
}

void Gfx::loadImage(const char* path, Texture* texture) {
  SDL_Surface *surface = IMG_Load(path);
  ENSURE(surface != NULL);
  texture->init(surface);
}

void drawCircle(Vec2f center, float radius, u32 pointCount) {
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(center.x, center.y);

  float angle = 0;
  float t = 2 * PI / ((float)(pointCount - 1));

  for (u32 i = 0; i < pointCount; ++i) {
    Vec2f p = center + Vec2f(cos(angle), sin(angle)) * radius;
    glVertex2f(p.x, p.y);
    angle += t;
  }
  glEnd();
}

void drawLine(Vec2f *points, usize pointCount, float width) {
  ASSERT(pointCount > 1);

  float lineWidth = width;
  glBegin(GL_TRIANGLES);

  Vec2f ab;
  Vec2f a0;
  Vec2f a1;

  {
    const Vec2f a = points[0];
    const Vec2f b = points[1];

    ab = a.vectorTo(b);
    const Vec2f normalAB = ab.ortho().normalized();
    const Vec2f dAB = (normalAB * lineWidth);
    a0 = a - dAB;
    a1 = a + dAB;
  }

  for (usize i = 2; i < pointCount; ++i) {
    const Vec2f &b = points[i - 1];
    const Vec2f &c = points[i];

    const Vec2f bc = b.vectorTo(c);
    const Vec2f normalBC = bc.ortho().normalized();
    const Vec2f dBC = (normalBC * lineWidth);
    const Vec2f c0 = c - dBC;
    const Vec2f c1 = c + dBC;

    const Vec2f miter =
        (ab.normalized() + bc.normalized()).normalized().ortho();
    const Vec2f m = miter * (lineWidth / miter.dotProduct(normalBC));
    const Vec2f b0 = b - m;
    const Vec2f b1 = b + m;

    glVertex2f(a0.x, a0.y);
    glVertex2f(a1.x, a1.y);
    glVertex2f(b1.x, b1.y);

    glVertex2f(b1.x, b1.y);
    glVertex2f(b0.x, b0.y);
    glVertex2f(a0.x, a0.y);

    ab = bc;
    a0 = b0;
    a1 = b1;
  }

  {
    const Vec2f b = points[pointCount - 1];

    const Vec2f normalAB = ab.ortho().normalized();
    const Vec2f dAB = (normalAB * lineWidth);
    const Vec2f b0 = b - dAB;
    const Vec2f b1 = b + dAB;

    glVertex2f(a0.x, a0.y);
    glVertex2f(a1.x, a1.y);
    glVertex2f(b1.x, b1.y);

    glVertex2f(b1.x, b1.y);
    glVertex2f(b0.x, b0.y);
    glVertex2f(a0.x, a0.y);
  }

  glEnd();
}

void clearScreen(ColorRGBA c) {
  glClearColor(c.r / 255.0f, c.g / 255.0f, c.b / 255.0f, c.a / 255.0f);
  glClear(GL_COLOR_BUFFER_BIT);
}

void setDrawColor(ColorRGBA c) {
  glColor4f(c.r / 255.0f, c.g / 255.0f, c.b / 255.0f, c.a / 255.0f);
}

void drawRect(int x, int y, int w, int h) {
  // FIXME: line end should be customizable to avoid drawing another segment
  // entirely
  Vec2f points[] = {
      {x, y}, {x, y + h}, {x + w, y + h}, {x + w, y}, {x, y}, {x, y + h},
  };

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  drawLine(points, ARRAY_SIZE(points), 0.5f);
}

void fillRect(float x, float y, float w, float h) {
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glRectf(x, y, x + w, y + h);
}
