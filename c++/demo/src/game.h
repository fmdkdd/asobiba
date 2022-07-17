#ifndef GAME_H
#define GAME_H

#include <SDL2/SDL.h>

#include "utils.h"

struct Controls;
struct Gfx;

struct AnimatedSprite {
  SDL_Texture *spritesheet;
  SDL_Rect *spriteRects;
  u32 spriteRectsCount;

  u32 animationStep;
  u32 animationSpeed;
  u32 animationStepCounter;

  void drawAt(SDL_GLContext gl, u32 x, u32 y);
  void step();
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

  bool collideWith(Rect &other) const;
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
  u32 ballSpeedIncreaseOnHit;

  void update(const Controls &controls);
};

struct Game {
  SDL_Texture *lemmingsSpritesheet;

  AnimatedSprite lemmings[10];

  Pong pong;

  void init(Gfx &gfx);
  void quit();
  void reset();

  void update(const Controls &controls);
  void render(Gfx &gfx);
};

#endif
