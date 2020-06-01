#include <dlfcn.h>
#include <SDL2/SDL_image.h>

#include "assert.h"
#include "sdl.h"
#include "game_api.h"

struct GameLib {
  void* handle;
  Game* state;
  GameAPI api;
};

void ReloadGame(GameLib& lib) {
  static const char* GAME_LIBRARY = "./libgame.so";

  printf("Reloading game\n");
  if (lib.handle)
    ASSERT(dlclose(lib.handle) == 0);
  lib.handle = dlopen(GAME_LIBRARY, RTLD_NOW);
  ASSERT(lib.handle != NULL);
  if (lib.handle) {
    GameAPI* api = (GameAPI*)dlsym(lib.handle, "GAME_API");
    ASSERT(api != NULL);
    lib.api = *api;
  }
}

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

  IMG_Init(IMG_INIT_PNG);

  GameLib gameLib = {};
  ReloadGame(gameLib);
  gameLib.state = gameLib.api.Init(renderer.renderer);

  while (true) {
    renderer.commitInputState();

    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;
      case SDL_KEYUP:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          goto done;

        else if (e.key.keysym.sym == SDLK_r) {
          ReloadGame(gameLib);
        }
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

    gameLib.api.Update(gameLib.state, renderer.renderer);

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

  gameLib.api.Quit(gameLib.state);

  IMG_Quit();

 done:
  return EXIT_SUCCESS;
}
