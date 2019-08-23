#include <SDL2/SDL.h>
#include "game.h"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t i32;

void die(const char *const msg) {
  perror(msg);
  exit(1);
}

void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

int main() {
  // Init SDL
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    sdl_die("Unable to initialize SDL");

  auto window =
    SDL_CreateWindow("SUPER QUELTRIS",
                     SDL_WINDOWPOS_UNDEFINED,
                     SDL_WINDOWPOS_UNDEFINED,
                     350, 500,
                     0);
  if (!window)
    sdl_die("Could not create window");

  auto renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
  if (!renderer)
    sdl_die("Could not create renderer");

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

    SDL_RenderPresent(renderer);
  }

 done:
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return EXIT_SUCCESS;
}
