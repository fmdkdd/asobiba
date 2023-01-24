#include "controls.h"

void Controls::init() {
  memset(mouseButtonHeld, false, sizeof(mouseButtonHeld));
  memset(mouseButtonPreviousHeld, false, sizeof(mouseButtonPreviousHeld));
  lastScreenMousePosition.x = 0;
  lastScreenMousePosition.y = 0;
  lastLogicalMousePosition.x = 0;
  lastLogicalMousePosition.y = 0;
  memset(keyHeld, false, sizeof(keyHeld));
  memset(keyPreviousHeld, false, sizeof(keyPreviousHeld));
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
