#ifndef GAME_H
#define GAME_H

#include <chrono>

#include <SDL2/SDL.h>
#include "utils.h"

struct App;

struct AnimatedSprite {
  SDL_Texture *spritesheet;
  SDL_Rect *spriteRects;
  u32 spriteRectsCount;

  u32 animationStep;
  u32 animationSpeed;
  u32 animationStepCounter;

  void drawAt(SDL_Renderer* renderer, u32 x, u32 y);
  void step(u32 ticks);
};

struct Point {
  s32 x;
  s32 y;
};

struct Rect {
  s32 x;
  s32 y;
  s32 w;
  s32 h;

  bool collideWith(Rect& other) const;
};

struct Pong {
  Rect arena;
  Rect player1Bat;
  Rect player2Bat;
  Rect ball;

  s32 player1Velocity;
  u32 player1Speed;
  u32 player1SpeedCounter;
  s32 player2Velocity;
  u32 player2Speed;
  u32 player2SpeedCounter;

  Point ballVelocity;

  u32 ballSpeed;
  u32 ballCounter;

  void update(const App& app, u32 ticks);
};

struct Game {
  SDL_Texture* lemmingsSpritesheet;

  AnimatedSprite lemmings[10];

  Pong pong;

  void init(App& app);
  void quit();
  void reset();

  void update(const App& app, u32 ticks);
  void render(App& app);
};

#endif
