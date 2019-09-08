#pragma once

#include <string>

#include <SDL2/SDL.h>

#include "types.h"

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

struct SDL {
  SDL() {
    if (SDL_Init(SDL_INIT_VIDEO) != 0)
      sdl_die("Unable to initialize SDL");
  }

  ~SDL() { SDL_Quit(); }
};

struct SDLWindow {
  SDL_Window *window;

  SDLWindow(const char* name, int width, int height)
    : window {SDL_CreateWindow(name,
                               SDL_WINDOWPOS_UNDEFINED,
                               SDL_WINDOWPOS_UNDEFINED,
                               width, height,
                               0)} {
    if (!window)
      sdl_die("Could not create window");
  }

  ~SDLWindow() { SDL_DestroyWindow(window); }
};

struct Color {
  u8 r;
  u8 g;
  u8 b;
  u8 a = 255;
};

struct SDLSurface {
  SDL_Surface *surface;

  explicit SDLSurface(SDL_Surface *s) : surface {s} {}

  SDLSurface(const SDLSurface &s) = delete;
  SDLSurface(SDLSurface &&s) = default;

  ~SDLSurface() { SDL_FreeSurface(surface); }
};

struct SDLTexture {
  SDL_Texture *texture;

  SDLTexture(SDL_Texture *t) : texture {t} {}

  ~SDLTexture() { SDL_DestroyTexture(texture); }
};

struct SDLRenderer;

struct SDLFont {
  SDLTexture texture;
  int width;
  int height;
  int mapping[256];

  SDLFont(const SDLRenderer &r, const std::string &file,
          const int width, const int height,
          const std::string &mapping);
  void draw(SDLRenderer &r, unsigned char c, int x, int y);
};

static constexpr const char *FONT_CHARS = " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?";

struct SDLRenderer {
  SDL_Renderer *renderer;
  SDLFont font;

  SDLRenderer(const SDLWindow& window, SDL_RendererFlags flags)
    : renderer {SDL_CreateRenderer(window.window, -1, flags)},
      font(*this, "font.bmp", 9, 16, FONT_CHARS) {
    if (!renderer)
      sdl_die("Could not create renderer");
  }

  ~SDLRenderer() { SDL_DestroyRenderer(renderer); }

  void present() {
    SDL_RenderPresent(renderer);
  }

  void set_scale(float x, float y) {
    SDL_RenderSetScale(renderer, x, y);
  }

  void set_draw_color(Color c) {
    SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, c.a);
  }

  void clear() { SDL_RenderClear(renderer); }

  void draw_line(int x1, int y1, int x2, int y2) {
    SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
  }

  void fill_rect(int x, int y, int w, int h) {
    SDL_Rect r {x,y,w,h};
    SDL_RenderFillRect(renderer, &r);
  }

  SDLTexture create_texture(SDLSurface &&s) const {
    return SDL_CreateTextureFromSurface(renderer, s.surface);
  }

  void copy(const SDLTexture &t, const SDL_Rect *src, const SDL_Rect *dst) {
    SDL_RenderCopy(renderer, t.texture, src, dst);
  }

  void text(const std::string &s, int x, int y) {
    for (auto c: s) {
      font.draw(*this, c, x, y);
      x += font.width-1;
    }
  }
};

// Load BMP file into SDL_Surface,

// convert surface into texture

// to draw
// renderCopy rect of char to renderer
