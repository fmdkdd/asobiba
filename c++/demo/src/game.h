#ifndef GAME_H
#define GAME_H

#include <SDL2/SDL.h>

#include "gfx.h"
#include "vec.h"

struct Controls;

struct Pong {
  Recti arena;
  Recti player1Bat;
  Recti player2Bat;
  Recti ball;

  s32 player1Velocity;
  u32 player1Speed;
  u32 player1SpeedCounter;
  s32 player2Velocity;
  u32 player2Speed;
  u32 player2SpeedCounter;

  Vec2i ballVelocity;

  u32 ballSpeed;
  u32 ballCounter;
  u32 ballSpeedIncreaseOnHit;

  void update(const Controls &controls);
};

struct Game {
  Texture lemmingsSpritesheet;

  AnimatedSprite lemmings[10];

  Pong pong;

  void init(Gfx &gfx);
  void quit();
  void reset();

  void update(const Controls &controls);
  void render(Gfx &gfx);
};

#endif
