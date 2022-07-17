#ifdef HOT_RELOAD
#include <dlfcn.h>
#include <signal.h>
#endif

#include "app.h"
#include "game_api.h"
#include "utils.h"

#ifdef HOT_RELOAD
void reloadGame(GameLib &lib) {
  static const char *GAME_LIBRARY = "./libgame.so";

  printf("Reloading game\n");
  if (lib.handle)
    ASSERT(dlclose(lib.handle) == 0);
  lib.handle = dlopen(GAME_LIBRARY, RTLD_NOW);
  if (lib.handle == NULL) {
    fprintf(stderr, "dlopen failed: %s\n", dlerror());
    exit(1);
  }
  if (lib.handle) {
    GameAPI *api = (GameAPI *)dlsym(lib.handle, "GAME_API");
    ASSERT(api != NULL);
    lib.api = *api;
  }
}

static GameLib *gGameLib;
static void onSignalUSR1(int) { reloadGame(*gGameLib); }
static void onHotReloadKey() { reloadGame(*gGameLib); }
#endif

int main() {
  App app;

  app.init();

  GameLib gameLib;
  gameLib.handle = NULL;
  gameLib.state = NULL;

#ifdef HOT_RELOAD
  reloadGame(gameLib);
#else
  gameLib.api = GAME_API;
#endif
  gameLib.state = gameLib.api.init(app.gfx);

#ifdef HOT_RELOAD
  gGameLib = &gameLib;
  signal(SIGUSR1, onSignalUSR1);
  app.hotReloadCallback = onHotReloadKey;
#endif

  app.gameLib = &gameLib;
  app.run();

  gameLib.api.quit(gameLib.state);

  app.quit();

  return EXIT_SUCCESS;
}
