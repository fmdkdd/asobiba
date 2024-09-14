#ifndef GAME_H
#define GAME_H

#include <SDL2/SDL.h>

#include "arena.h"
#include "vec.h"

enum class SceneId;
struct Scene;
struct Gfx;

struct Controls;

struct Game {
  Arena m_arena;

  Scene* m_pCurrentScene;
  SceneId m_nextScene;

  u64 time;
  bool isPaused;

  // Settings
  bool m_useLemmings;

  void init(Gfx &gfx);
  void quit();
  void reset();

  void update(const Controls &controls, Gfx& gfx);
  void render(Gfx &gfx);
  void renderImGui();

  void changeScene(SceneId sceneId);
};

#endif
