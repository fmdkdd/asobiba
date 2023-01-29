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
  bool gamePreviousMouseButtonDown[MOUSE_BUTTON_COUNT];
  bool renderPreviousMouseButtonDown[MOUSE_BUTTON_COUNT];
  bool mouseButtonDown[MOUSE_BUTTON_COUNT];

  bool gamePreviousKeyDown[KEY_COUNT];
  bool renderPreviousKeyDown[KEY_COUNT];
  bool keyDown[KEY_COUNT];

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

  void swapGameInputState() {
    for (usize i = 0; i < K_ARRAY_SIZE(mouseButtonDown); ++i) {
      gamePreviousMouseButtonDown[i] = mouseButtonDown[i];
    }
    for (usize i = 0; i < K_ARRAY_SIZE(keyDown); ++i) {
      gamePreviousKeyDown[i] = keyDown[i];
    }
  }

  void swapRenderInputState() {
    for (usize i = 0; i < K_ARRAY_SIZE(mouseButtonDown); ++i) {
      renderPreviousMouseButtonDown[i] = mouseButtonDown[i];
    }
    for (usize i = 0; i < K_ARRAY_SIZE(keyDown); ++i) {
      renderPreviousKeyDown[i] = keyDown[i];
    }
  }

  bool wasGameMouseButtonPressed(u8 button) const {
    return mouseButtonDown[button] && !gamePreviousMouseButtonDown[button];
  }

  bool wasGameMouseButtonReleased(u8 button) const {
    return !mouseButtonDown[button] && gamePreviousMouseButtonDown[button];
  }

  bool wasRenderMouseButtonPressed(u8 button) const {
    return mouseButtonDown[button] && !renderPreviousMouseButtonDown[button];
  }

  bool wasRenderMouseButtonReleased(u8 button) const {
    return !mouseButtonDown[button] && renderPreviousMouseButtonDown[button];
  }

  bool isMouseButtonDown(u8 button) const { return mouseButtonDown[button]; }

  void onMouseButtonDown(u8 button) {
    mouseButtonDown[button] = true;
  }

  void onMouseButtonUp(u8 button) {
    mouseButtonDown[button] = false;
  }

  Key mapKeycodeToKey(SDL_Keycode keycode) const;

  void onKeyUp(SDL_Keycode keycode) {
    Key key = mapKeycodeToKey(keycode);
    if (key != KEY_IGNORED)
      keyDown[key] = false;
  }

  void onKeyDown(SDL_Keycode keycode) {
    Key key = mapKeycodeToKey(keycode);
    if (key != KEY_IGNORED)
      keyDown[key] = true;
  }

  bool wasGameKeyPressed(Key key) const {
    return keyDown[key] && !gamePreviousKeyDown[key];
  }

  bool wasGameKeyReleased(Key key) const {
    return !keyDown[key] && gamePreviousKeyDown[key];
  }

  bool wasRenderKeyPressed(Key key) const {
    return keyDown[key] && !renderPreviousKeyDown[key];
  }

  bool wasRenderKeyReleased(Key key) const {
    return !keyDown[key] && renderPreviousKeyDown[key];
  }

  bool isKeyDown(Key key) const { return keyDown[key]; }

};

#endif
