#pragma once

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

struct SDLRenderer {
  SDL_Renderer *renderer;

  SDLRenderer(SDLWindow& window, SDL_RendererFlags flags)
    : renderer {SDL_CreateRenderer(window.window, -1, flags)} {
    if (!renderer)
      sdl_die("Could not create renderer");
  }

  ~SDLRenderer() { SDL_DestroyRenderer(renderer); }

  void present() {
    SDL_RenderPresent(renderer);
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
};
