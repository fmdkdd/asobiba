#include "controls.h"

void Controls::init() {
  memset(mouseButtonDown, false, sizeof(mouseButtonDown));
  memset(gamePreviousMouseButtonDown, false, sizeof(gamePreviousMouseButtonDown));
  memset(renderPreviousMouseButtonDown, false, sizeof(renderPreviousMouseButtonDown));
  lastScreenMousePosition.x = 0;
  lastScreenMousePosition.y = 0;
  lastLogicalMousePosition.x = 0;
  lastLogicalMousePosition.y = 0;
  memset(keyDown, false, sizeof(keyDown));
  memset(gamePreviousKeyDown, false, sizeof(gamePreviousKeyDown));
  memset(renderPreviousKeyDown, false, sizeof(renderPreviousKeyDown));
}

void Controls::quit() {}

Key Controls::mapKeycodeToKey(SDL_Keycode keycode) const {
  switch (keycode) {
  case SDLK_r:
    return KEY_PLAYER1_UP;
  case SDLK_x:
    return KEY_PLAYER1_DOWN;
  case SDLK_e:
    return KEY_PLAYER2_UP;
  case SDLK_m:
    return KEY_PLAYER2_DOWN;
  case SDLK_p:
    return KEY_PAUSE;
  default:
    return KEY_IGNORED;
  }
}
