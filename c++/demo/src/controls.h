#ifndef CONTROLS_H
#define CONTROLS_H

#include <SDL2/SDL.h>

#include "utils.h"

enum Key {
  KEY_IGNORED,

  KEY_PLAYER1_UP,
  KEY_PLAYER1_DOWN,
  KEY_PLAYER2_UP,
  KEY_PLAYER2_DOWN,

  KEY_PAUSE,

  KEY_COUNT,
};

struct Controls {
  SDL_Point lastScreenMousePosition;
  SDL_Point lastLogicalMousePosition;

  static const usize MOUSE_BUTTON_COUNT = 5;
  bool mouseButtonPreviousHeld[MOUSE_BUTTON_COUNT];
  bool mouseButtonHeld[MOUSE_BUTTON_COUNT];

  bool keyPreviousHeld[KEY_COUNT];
  bool keyHeld[KEY_COUNT];

  void init();
  void quit();

  void updateScreenMousePosition(int x, int y) {
    lastScreenMousePosition.x = x;
    lastScreenMousePosition.y = y;
  }

  void updateLogicalMousePosition(int x, int y) {
    lastLogicalMousePosition.x = x;
    lastLogicalMousePosition.y = y;
  }

  void swapInputState() {
    for (usize i = 0; i < K_ARRAY_SIZE(mouseButtonHeld); ++i) {
      mouseButtonPreviousHeld[i] = mouseButtonHeld[i];
    }
    for (usize i = 0; i < K_ARRAY_SIZE(keyHeld); ++i) {
      keyPreviousHeld[i] = keyHeld[i];
    }
  }

  bool isMouseButtonPressed(u8 button) const {
    return mouseButtonHeld[button] && !mouseButtonPreviousHeld[button];
  }

  bool isMouseButtonHeld(u8 button) const { return mouseButtonHeld[button]; }

  void updateMouseButton(u8 button, u8 state) {
    if (state == SDL_RELEASED)
      mouseButtonHeld[button] = false;
    else
      mouseButtonHeld[button] = true;
  }

  Key mapKeycodeToKey(SDL_Keycode keycode) const;

  void onKeyUp(SDL_Keycode keycode) {
    Key key = mapKeycodeToKey(keycode);
    if (key != KEY_IGNORED)
      keyHeld[key] = false;
  }

  void onKeyDown(SDL_Keycode keycode) {
    Key key = mapKeycodeToKey(keycode);
    if (key != KEY_IGNORED)
      keyHeld[key] = true;
  }

  bool isKeyPressed(Key key) const {
    return keyHeld[key] && !keyPreviousHeld[key];
  }

  bool isKeyHeld(Key key) const { return keyHeld[key]; }

};

#endif
