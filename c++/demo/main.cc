#include <chrono>
#include <vector>

#include "assert.h"
#include "sdl.h"
#include <SDL2/SDL_image.h>

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

  void DrawAt(SDLRenderer& renderer, u32 x, u32 y) {
    SDL_Rect dst;
    dst.x = x;
    dst.y = y;
    dst.w = 40;
    dst.h = 40;
    ASSERT(animationStep < rects.size());
    SDL_RenderCopy(renderer.renderer, spritesheet, &rects[animationStep], &dst);
  }

  void Step() {
    animationStep++;
    if (animationStep >= rects.size())
      animationStep = 0;
  }
};



int main() {
  const u32 logical_width  = 800;
  const u32 logical_height = 450;
  const u32 scale = 1;

  const u32 window_width = logical_width * scale;
  const u32 window_height = logical_height * scale;

  SDL sdl;
  SDLWindow window("SDL demo", window_width, window_height);
  SDLRenderer renderer(window, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  renderer.set_scale(scale, scale);
  SDL_RenderSetIntegerScale(renderer.renderer, SDL_TRUE);
  SDL_RenderSetLogicalSize(renderer.renderer, logical_width, logical_height);

  auto last_frame = std::chrono::high_resolution_clock::now();

  IMG_Init(IMG_INIT_PNG);

  SDL_Texture* lemmingsSpritesheet = SDL_CreateTextureFromSurface(renderer.renderer,
                                                                  IMG_Load("lemmings-spritesheet.png"));

  u32 frame = 0;

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

  AnimatedSprite lemmings[10];
  size_t lemmingIndex = 0;

  for (size_t i=0; i < 10; ++i) {
    lemmings[i] = walkinLemmin;
    lemmings[i].animationStep = rand() % 7;
  }

  while (true) {
    renderer.commitInputState();

    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;
      case SDL_KEYUP:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          goto done;
        break;

      case SDL_MOUSEMOTION:
        renderer.updateMousePosition(e.motion.x, e.motion.y);
        break;

      case SDL_MOUSEBUTTONDOWN:
      case SDL_MOUSEBUTTONUP:
        renderer.updateMouseButton(e.button.button, e.button.state);
        break;
      }
    }

    auto now = std::chrono::high_resolution_clock::now();
    auto dt = now - last_frame;
    last_frame = now;
    double dt_ms = (double) std::chrono::nanoseconds(dt).count() / 1'000'000;

    renderer.set_draw_color({30,30,30});
    renderer.clear();

    renderer.set_draw_color({255,255,255});
    renderer.draw_rect(0,0, logical_width, logical_height);

    renderer.text("Hello!", 16, 16);

    renderer.boxed_text("I'm a text box", 16, 48);

    char buf[256];
    snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
    renderer.text(buf, 160, 20);

    int mouse_x;
    int mouse_y;
    SDL_GetMouseState(&mouse_x, &mouse_y);
    snprintf(buf, sizeof(buf), "mouse: x: %d y: %d", mouse_x, mouse_y);
    renderer.text(buf, 160, 40);

    snprintf(buf, sizeof(buf), "logical mouse: x: %d y: %d", renderer.lastMousePosition.x, renderer.lastMousePosition.y);
    renderer.text(buf, 120, 80);

    renderer.draw_rect(renderer.lastMousePosition.x, renderer.lastMousePosition.y, 10, 10);

    if (renderer.button("And I'm a button", 16, 128)) {
      renderer.text("Stop clicking!", 16, 144);
    }

    const bool isFullscreen = SDL_GetWindowFlags(window.window) & SDL_WINDOW_FULLSCREEN_DESKTOP;

    if (isFullscreen) {
      if (renderer.button("Windowed", 16, 300)) {
        SDL_SetWindowFullscreen(window.window, 0);
      }
    }
    else {
      if (renderer.button("Fullscreen", 16, 300)) {
        SDL_SetWindowFullscreen(window.window, SDL_WINDOW_FULLSCREEN_DESKTOP);
      }
    }

    frame++;
    for (int i=0; i < 10; ++i) {
      if (frame % 6 == 0) {
        lemmings[i].Step();
      }
      lemmings[i].DrawAt(renderer, 10 + 20 * i, 200);
    }

    // TODO:
    // - Live reloading with game compiled as dynamically loaded lib
    // - Drag&drop
    // - Dialog boxes
    // - Menu with 50% alpha overlay
    // - Do not structure too much in order to leave
    //   maximum flexiblity for prototyping (no framework)
    // - UI animations?
    // - Screen shake
    // - ImGui integration

    renderer.present();
  }

  SDL_DestroyTexture(lemmingsSpritesheet);

  IMG_Quit();

 done:
  return EXIT_SUCCESS;
}
