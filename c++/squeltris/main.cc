#include <chrono>

#include "game.h"
#include "sdl.h"

int main() {
  int scale = 3;

  SDL sdl;
  SDLWindow window("SUPER QUELTRIS", WINDOW_WIDTH*scale, WINDOW_HEIGHT*scale);
  SDLRenderer renderer(window, SDL_RENDERER_PRESENTVSYNC);
  renderer.set_scale(scale, scale);

  Game game;
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

      game.sdl_event(e);
    }

    auto now = std::chrono::high_resolution_clock::now();
    auto dt = now - last_frame;
    last_frame = now;
    double dt_s = (double) std::chrono::nanoseconds(dt).count() / 1'000'000'000;
    game.update(dt_s);
    game.render(renderer);

    renderer.present();
  }

 done:
  return EXIT_SUCCESS;
}
