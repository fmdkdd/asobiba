#ifndef APP_H
#define APP_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "utils.h"

struct App;
struct GameLib;

struct ColorRGBA {
  u8 r;
  u8 g;
  u8 b;
  u8 a;
};

struct Font {
  SDL_Texture *texture;
  int width;
  int height;
  u8 charIndex[256];

  void init(const App &app, const char *file, int width, int height,
            const char *mapping, usize charMappingLength);
  void quit();

  void draw(App &app, unsigned char c, int x, int y);
};

enum Key {
  KEY_PLAYER1_UP,
  KEY_PLAYER1_DOWN,
  KEY_PLAYER2_UP,
  KEY_PLAYER2_DOWN,

  KEY_COUNT,
};

struct App {
  SDL_Window *window;
  SDL_Renderer *renderer;
  Font font;

  bool running;

  GameLib *gameLib;

  void (*hotReloadCallback)();

  SDL_Point lastMousePosition;

  static const usize MOUSE_BUTTON_COUNT = 5;
  bool mouseButtonPreviousHeld[MOUSE_BUTTON_COUNT];
  bool mouseButtonHeld[MOUSE_BUTTON_COUNT];

  bool keyPreviousHeld[KEY_COUNT];
  bool keyHeld[KEY_COUNT];

  void init();
  void run();
  void quit();

  u64 getHostRefreshRate();
  void updateInputs();

  void updateMousePosition(int x, int y) {
    lastMousePosition.x = x;
    lastMousePosition.y = y;
  }

  void swapInputState() {
    for (size_t i = 0; i < ARRAY_SIZE(mouseButtonHeld); ++i) {
      mouseButtonPreviousHeld[i] = mouseButtonHeld[i];
    }
    for (size_t i = 0; i < ARRAY_SIZE(keyHeld); ++i) {
      keyPreviousHeld[i] = keyHeld[i];
    }
  }

  bool isMouseButtonPressed(u8 button) const {
    return mouseButtonHeld[button] && !mouseButtonPreviousHeld[button];
  }

  bool isMouseButtonHeld(u8 button) const { return mouseButtonHeld[button]; }

  void updateMouseButton(u8 button, u8 state) {
    if (state == SDL_RELEASED)
      mouseButtonHeld[button] = false;
    else
      mouseButtonHeld[button] = true;
  }

  Key mapKeycodeToKey(SDL_Keycode keycode) const {
    switch (keycode) {
    case SDLK_r:
      return KEY_PLAYER1_UP;
    case SDLK_x:
      return KEY_PLAYER1_DOWN;
    case SDLK_e:
      return KEY_PLAYER2_UP;
    case SDLK_m:
      return KEY_PLAYER2_DOWN;
    default:
      UNREACHABLE();
    }
  }

  void updateKey(SDL_Keycode keycode, bool released) {
    Key key = mapKeycodeToKey(keycode);
    if (released)
      keyHeld[key] = false;
    else
      keyHeld[key] = true;
  }

  bool isKeyPressed(Key key) const {
    return keyHeld[key] && !keyPreviousHeld[key];
  }

  bool isKeyHeld(Key key) const { return keyHeld[key]; }

  void setRenderDrawColor(ColorRGBA c) {
    SDL_SetRenderDrawColor(renderer, c.r, c.g, c.b, c.a);
  }

  void renderClear() { SDL_RenderClear(renderer); }

  void drawLine(int x1, int y1, int x2, int y2) {
    SDL_RenderDrawLine(renderer, x1, y1, x2, y2);
  }

  void fillRect(int x, int y, int w, int h) {
    SDL_Rect r{x, y, w, h};
    SDL_RenderFillRect(renderer, &r);
  }

  void drawRect(int x, int y, int w, int h) {
    SDL_Rect r{x, y, w, h};
    SDL_RenderDrawRect(renderer, &r);
  }

  SDL_Texture *loadImage(const char *path) {
    SDL_Surface *surface = IMG_Load(path);
    ENSURE(surface != NULL);
    return SDL_CreateTextureFromSurface(renderer, surface);
  }

  u32 text(const char *s, int x, int y);
  SDL_Rect boxedText(const char *s, u32 x, u32 y);
  bool button(const char *s, u32 x, u32 y);
};

#endif
