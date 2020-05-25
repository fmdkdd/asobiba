#include <chrono>

#include "sdl.h"

int main() {
  const int scale = 2;

  SDL sdl;
  SDLWindow window("SDL demo", 640, 420);
  SDLRenderer renderer(window, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  renderer.set_scale(scale, scale);

  auto last_frame = std::chrono::high_resolution_clock::now();

  while (true) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;
      case SDL_KEYUP:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          goto done;
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
    renderer.text("Hello!", 16, 16);

    renderer.boxed_text("I'm a text box", 16, 48);

    char buf[32];
    snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
    renderer.text(buf, 160, 20);

    int mouse_x;
    int mouse_y;
    SDL_GetMouseState(&mouse_x, &mouse_y);
    snprintf(buf, sizeof(buf), "mouse: x: %d y: %d", mouse_x, mouse_y);
    renderer.text(buf, 160, 40);

    if (renderer.button("And I'm a button", 16, 128)) {
      renderer.text("Stop clicking!", 16, 144);
    }

    // TODO:
    // - Live reloading with game compiled as dynamically loaded lib
    // - Use textures (since drawing with SDL is limited, blitting PNGs
    //   should be the default besides text)
    // - Fullscreen should be use closest integer scaling, not extend screen
    // - Screen shake
    // - ImGui integration
    // - ASSERT/VERIFY macros

    renderer.present();
  }

 done:
  return EXIT_SUCCESS;
}
