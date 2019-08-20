#include <SDL2/SDL.h>

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

enum Color {
            RED, GREEN, BLUE, YELLOW
};

struct Board {
  Color color[7*10];

  void draw(SDL_Renderer *r) {
    for (int y=0; y < 10; ++y) {
      for (int x=0; x < 7; ++x) {
        switch (color[y*7+x]) {
        case RED: SDL_SetRenderDrawColor(r, 255, 0, 0, SDL_ALPHA_OPAQUE); break;
        case GREEN: SDL_SetRenderDrawColor(r, 0, 255, 0, SDL_ALPHA_OPAQUE); break;
        case BLUE: SDL_SetRenderDrawColor(r, 0, 0, 255, SDL_ALPHA_OPAQUE); break;
        case YELLOW: SDL_SetRenderDrawColor(r, 255, 255, 0, SDL_ALPHA_OPAQUE); break;
        }

        SDL_Rect rec = {x*12, y*12, 10, 10};
        SDL_RenderFillRect(r, &rec);
      }
    }
  }
};

int main(int argc, char* argv[]) {

  // Init SDL
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    sdl_die("Unable to initialize SDL");

  SDL_Window *window =
    SDL_CreateWindow("SUPER QUELTRIS",
                     SDL_WINDOWPOS_UNDEFINED,
                     SDL_WINDOWPOS_UNDEFINED,
                     224, 256,
                     0);
  if (!window)
    sdl_die("Could not create window");

  SDL_Renderer *renderer =
    SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
  if (!renderer)
    sdl_die("Could not create renderer");

  Board board;

  while (true) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;

      case SDL_KEYDOWN:
        switch (e.key.keysym.scancode) {
        default: break;
        }
        break;

      case SDL_KEYUP:
        switch (e.key.keysym.scancode) {
        case SDL_SCANCODE_ESCAPE: goto done;

        default: break;
        }
        break;
      }
    }

    // Update



    // Draw
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(renderer);

    board.draw(renderer);

    SDL_RenderPresent(renderer);
  }

 done:

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return EXIT_SUCCESS;
}
