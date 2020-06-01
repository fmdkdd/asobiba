#include "game.h"

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "assert.h"
#include "game_api.h"

void AnimatedSprite::DrawAt(SDL_Renderer* renderer, u32 x, u32 y) {
  SDL_Rect dst;
  dst.x = x;
  dst.y = y;
  dst.w = 40;
  dst.h = 40;
  ASSERT(animationStep < rects.size());
  SDL_RenderCopy(renderer, spritesheet, &rects[animationStep], &dst);
}

void AnimatedSprite::Step() {
  animationStep++;
  if (animationStep >= rects.size())
    animationStep = 0;
}

void Game::Init(SDL_Renderer* renderer) {
  SDL_Texture* lemmingsSpritesheet = SDL_CreateTextureFromSurface(renderer,
                                                                  IMG_Load("lemmings-spritesheet.png"));

  frame = 0;

  AnimatedSprite walkinLemmin(lemmingsSpritesheet);
  {
    SDL_Rect src;
    src.y = 0;
    src.w = 20;
    src.h = 20;
    for (size_t i=0; i < 7; ++i) {
      walkinLemmin.AddAnimationStep(src);
      src.x += 20;
    }
  }

  for (size_t i=0; i < 10; ++i) {
    lemmings[i] = walkinLemmin;
    lemmings[i].animationStep = rand() % 7;
  }

  last_frame = std::chrono::high_resolution_clock::now();
}

void Game::Quit() {
  SDL_DestroyTexture(lemmingsSpritesheet);
}

void Game::Update(SDL_Renderer* renderer) {
  auto now = std::chrono::high_resolution_clock::now();
  auto dt = now - last_frame;
  last_frame = now;
  double dt_ms = (double) std::chrono::nanoseconds(dt).count() / 1'000'000;

  SDL_SetRenderDrawColor(renderer, 30,30,30,255);
  SDL_RenderClear(renderer);

  SDL_SetRenderDrawColor(renderer, 255,255,255,255);

  int logicalWidth;
  int logicalHeight;
  SDL_RenderGetLogicalSize(renderer, &logicalWidth, &logicalHeight);
  {
    SDL_Rect r {0,0, logicalWidth, logicalHeight};
    SDL_RenderDrawRect(renderer, &r);
  }

  //renderer.text("Hello!", 16, 16);

  //renderer.boxed_text("I'm a text box", 16, 48);

  // char buf[256];
  // snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
  // renderer.text(buf, 160, 20);

  // int mouse_x;
  // int mouse_y;
  // SDL_GetMouseState(&mouse_x, &mouse_y);
  // snprintf(buf, sizeof(buf), "mouse: x: %d y: %d", mouse_x, mouse_y);
  // renderer.text(buf, 160, 40);

  // snprintf(buf, sizeof(buf), "logical mouse: x: %d y: %d", renderer.lastMousePosition.x, renderer.lastMousePosition.y);
  // renderer.text(buf, 120, 80);

  // renderer.draw_rect(renderer.lastMousePosition.x, renderer.lastMousePosition.y, 10, 10);

  // if (renderer.button("And I'm a button", 16, 128)) {
  //   renderer.text("Stop clicking!", 16, 144);
  // }

  frame++;
  for (int i=0; i < 10; ++i) {
    if (frame % 6 == 0) {
      lemmings[i].Step();
    }
    lemmings[i].DrawAt(renderer, 10 + 20 * i, 200);
  }
}


Game* Game_Init(SDL_Renderer* renderer) {
  Game* game = new Game;
  game->Init(renderer);
  return game;
}

void Game_Quit(Game* game) {
  game->Quit();
  delete game;
}

void Game_Update(Game* game, SDL_Renderer* renderer) {
  game->Update(renderer);
}

const GameAPI GAME_API = {
  .Init = Game_Init,
  .Quit = Game_Quit,
  .Update = Game_Update,
};
