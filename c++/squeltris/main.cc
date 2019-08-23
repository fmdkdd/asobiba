#include "game.h"
#include "sdl.h"

int main() {
  SDL sdl;
  SDLWindow window("SUPER QUELTRIS", 350, 500);
  SDLRenderer renderer(window, SDL_RENDERER_PRESENTVSYNC);

  Game game;

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

    game.update();
    game.render(renderer);

    renderer.present();
  }

 done:
  return EXIT_SUCCESS;
}
