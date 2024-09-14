#ifndef SCENE_H
#define SCENE_H

#include <new>

#include "arena.h"
#include "gfx.h"
#include "game.h"
#include "controls.h"

enum class SceneId;

struct Scene {
  Game *m_pGame;
  SceneId m_id;

  virtual void update(const Controls &controls) = 0;
  virtual void render(Gfx &gfx) = 0;
  virtual void reset() = 0;
  virtual void enter(Gfx &gfx) = 0;
  virtual void leave() = 0;
  virtual void destroy(Arena &arena) = 0;
};

typedef Scene* (*SceneCreateFunc)(Arena&);

#define SCENE_IMPL(S)                                                   \
  static Scene *create(Arena &arena) {                                  \
    void *p = arena.push(sizeof(S));                                    \
    return new (p) S;                                                   \
  }                                                                     \
                                                                        \
  void destroy(Arena &arena) override { arena.pop(sizeof(S)); }         \
                                                                        \
  void update(const Controls &controls) override;                       \
  void render(Gfx &gfx) override;                                       \
  void reset() override;                                                \
  void enter(Gfx &gfx) override;                                        \
  void leave() override;

// -- Scenes

#define FOR_EACH_SCENE(DO) \
  DO(None)                 \
  DO(TitleScreen)          \
  DO(Settings)             \
  DO(Demo)                 \

struct SceneTitleScreen : Scene {
  u32 m_state;
  u32 m_timer;
  u32 m_buttonsOffsetX;
  bool m_buttonsEnabled;

  SCENE_IMPL(SceneTitleScreen)
};

struct SceneSettings : Scene {
  SCENE_IMPL(SceneSettings)
};


struct Pong {
  Recti arena;
  Recti player1Bat;
  Recti player2Bat;
  Recti ball;

  s32 player1Velocity;
  u32 player1Speed;
  u32 player1SpeedCounter;
  s32 player2Velocity;
  u32 player2Speed;
  u32 player2SpeedCounter;

  Vec2i ballVelocity;

  u32 ballSpeed;
  u32 ballCounter;
  u32 ballSpeedIncreaseOnHit;
  u32 ballSpeedMax;

  void update(const Controls &controls);
};

struct SceneDemo : Scene {
  Texture lemmingsSpritesheet;

  AnimatedSprite lemmings[10];

  Pong pong;

  SCENE_IMPL(SceneDemo)
};


struct SceneNone : Scene {
  SCENE_IMPL(SceneNone)
};

inline void SceneNone::enter(Gfx& gfx) { K_UNUSED(gfx); }
inline void SceneNone::leave() { }
inline void SceneNone::reset() { }
inline void SceneNone::update(const Controls &controls) { K_UNUSED(controls); }
inline void SceneNone::render(Gfx &gfx) { K_UNUSED(gfx); }


#define DEF_SCENE_ID(id) id,
enum class SceneId {
  FOR_EACH_SCENE(DEF_SCENE_ID)
};

extern const SceneCreateFunc sSceneTable[];

#endif
