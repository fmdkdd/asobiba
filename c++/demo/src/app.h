#ifndef APP_H
#define APP_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_opengl.h>

#include "controls.h"
#include "gfx.h"
#include "utils.h"

struct GameLib;

struct StatHistory {
  float values[1024];
  usize count;
  usize index;

  float max;
  float min;

  void init();

  void push(float value);
  float getAverage() const;
};

struct App {
  u32 logicalWidth;
  u32 logicalHeight;

  u32 displayWidth;
  u32 displayHeight;

  StatHistory frameTimeHistory;
  StatHistory updateTimeHistory;
  StatHistory renderTimeHistory;

  SDL_Window *window;
  SDL_GLContext glContext;

  Controls controls;
  Gfx gfx;

  bool running;

  GameLib *gameLib;

  void (*hotReloadCallback)();

  void init();
  void run();
  void quit();

  void initImGui();
  void quitImGui();
  void drawImGui();

  u64 getHostRefreshRate();
  void updateInputs();
};

#endif
