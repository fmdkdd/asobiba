#ifndef GAME_H
#define GAME_H

#include <chrono>
#include <vector>

#include <SDL2/SDL.h>
#include "types.h"

struct AnimatedSprite {
  SDL_Texture *spritesheet;
  std::vector<SDL_Rect> rects;
  size_t animationStep;

  AnimatedSprite()
    : AnimatedSprite(NULL)
  {}

  explicit AnimatedSprite(SDL_Texture* spritesheet)
    : spritesheet(spritesheet)
    , animationStep(0)
  {}

  void AddAnimationStep(SDL_Rect rect) {
    rects.push_back(rect);
  }

  void DrawAt(SDL_Renderer* renderer, u32 x, u32 y);
  void Step();
};

struct Game {
  SDL_Texture* lemmingsSpritesheet;
  u32 frame;
  std::chrono::time_point<std::chrono::high_resolution_clock> last_frame;

  AnimatedSprite lemmings[10];

  void Init(SDL_Renderer* renderer);
  void Quit();

  void Update(SDL_Renderer* renderer);
};

#endif
