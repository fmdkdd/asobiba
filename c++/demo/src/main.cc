#ifdef HOT_RELOAD
#include <dlfcn.h>
#include <signal.h>
#endif

#include "app.h"
#include "game_api.h"
#include "utils.h"

#ifdef HOT_RELOAD
void ReloadGame(GameLib &lib) {
  static const char *GAME_LIBRARY = "./libgame.so";

  if (lib.handle)
    K_ASSERT(dlclose(lib.handle) == 0);
  lib.handle = dlopen(GAME_LIBRARY, RTLD_NOW);
  if (lib.handle == NULL) {
    fprintf(stderr, "dlopen failed: %s\n", dlerror());
    exit(1);
  }
  if (lib.handle) {
    GameAPI *api = (GameAPI *)dlsym(lib.handle, "GAME_API");
    K_ASSERT(api != NULL);
    lib.api = *api;
  }
  printf("Game lib reloaded\n");
}

static GameLib *gGameLib;
static void OnSignalUSR1(int) { ReloadGame(*gGameLib); }
static void OnHotReloadKey() { ReloadGame(*gGameLib); }
#endif

int main() {
  App app;

  app.Init();

  GameLib gameLib;
  gameLib.handle = NULL;
  gameLib.state = NULL;

#ifdef HOT_RELOAD
  ReloadGame(gameLib);
#else
  gameLib.api = GAME_API;
#endif
  gameLib.state = gameLib.api.init(app.m_Gfx);

#ifdef HOT_RELOAD
  gGameLib = &gameLib;
  signal(SIGUSR1, OnSignalUSR1);
  app.m_HotReloadCallback = OnHotReloadKey;
#endif

  app.m_GameLib = &gameLib;
  app.Run();

  gameLib.api.quit(gameLib.state);

  app.Quit();

  return EXIT_SUCCESS;
}
