#pragma once

#include <string>

#include <SDL2/SDL.h>

#include "types.h"

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

struct SDL {
  SDL_GameController* gameController;

  SDL() {
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER) != 0)
      sdl_die("Unable to initialize SDL");

    gameController = NULL;
    for (int i = 0; i < SDL_NumJoysticks(); ++i) {
      if (SDL_IsGameController(i)) {
        gameController = SDL_GameControllerOpen(i);
        if (gameController) {
          break;
        } else {
          fprintf(stderr, "Could not open gamecontroller %i: %s\n", i, SDL_GetError());
        }
      }
    }
  }

  ~SDL() {
    SDL_GameControllerClose(gameController);
    SDL_Quit();
  }
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

static const char *FONT_CHARS = " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?";

struct SDLRenderer {
  SDL_Renderer *renderer;
  SDLFont font;

  SDL_Point lastMousePosition;
  bool mouseButtonPreviousHeld[5];
  bool mouseButtonHeld[5];

  SDLRenderer(const SDLWindow& window, u32 flags)
    : renderer {SDL_CreateRenderer(window.window, -1, flags)},
      font(*this, "font.bmp", 9, 16, FONT_CHARS) {
    if (!renderer)
      sdl_die("Could not create renderer");
  }

  ~SDLRenderer() { SDL_DestroyRenderer(renderer); }

  void present() {
    SDL_RenderPresent(renderer);
  }

  void updateMousePosition(int x, int y) {
    lastMousePosition.x = x;
    lastMousePosition.y = y;
  }

  void commitInputState() {
    // TODO: ARRAY_SIZE
    for (size_t i = 0; i < 5; ++i) {
      mouseButtonPreviousHeld[i] = mouseButtonHeld[i];
    }
  }

  bool isMouseButtonPressed(u8 button) {
    return mouseButtonHeld[button] && !mouseButtonPreviousHeld[button];
  }

  void updateMouseButton(u8 button, u8 state) {
    if (state == SDL_RELEASED)
      mouseButtonHeld[button] = false;
    else
      mouseButtonHeld[button] = true;
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

  void draw_rect(int x, int y, int w, int h) {
    SDL_Rect r {x,y,w,h};
    SDL_RenderDrawRect(renderer, &r);
  }

  SDLTexture create_texture(SDLSurface &&s) const {
    return SDL_CreateTextureFromSurface(renderer, s.surface);
  }

  void copy(const SDLTexture &t, const SDL_Rect *src, const SDL_Rect *dst) {
    SDL_RenderCopy(renderer, t.texture, src, dst);
  }

  u32 text(const std::string &s, int x, int y);
  SDL_Rect boxed_text(const std::string &s, u32 x, u32 y);
  bool button(const std::string &s, u32 x, u32 y);
};

// Load BMP file into SDL_Surface,

// convert surface into texture

// to draw
// renderCopy rect of char to renderer
