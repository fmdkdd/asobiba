#include "game.h"

#include "imgui.h"

#include "controls.h"
#include "game_api.h"
#include "gfx.h"
#include "scene.h"
#include "utils.h"

// TODO: replace malloc with arena
// TODO: view arena usage
// TODO: title anim + animSequence objects

void Game::init(Gfx &gfx) {
  K_UNUSED(gfx);

  m_arena.init(1024);

  m_pCurrentScene = SceneNone::create(m_arena);
  m_nextScene = SceneId::TitleScreen;

  reset();
}

void Game::quit() {
  m_arena.quit();
}

void Game::reset() {
  time = 0;
  isPaused = false;
  m_useLemmings = true;

  m_pCurrentScene->reset();
}

void Game::update(const Controls &controls, Gfx& gfx) {
  if (m_nextScene != SceneId::None) {
    m_pCurrentScene->leave();
    m_pCurrentScene->destroy(m_arena);
    m_pCurrentScene = sSceneTable[u32(m_nextScene)](m_arena);
    m_pCurrentScene->m_pGame = this;
    m_pCurrentScene->m_id = m_nextScene;
    m_nextScene = SceneId::None;
    m_pCurrentScene->enter(gfx);
    m_pCurrentScene->reset();
  }

  m_pCurrentScene->update(controls);
}

void Game::changeScene(SceneId sceneId) {
  K_ASSERT(m_nextScene == SceneId::None);
  m_nextScene = sceneId;
}

void Game::render(Gfx &gfx) {
  m_pCurrentScene->render(gfx);
}

void Game::renderImGui() {
  ImGui::Text("State: %d", (int)m_pCurrentScene->m_id);
  //ImGui::Text("Ball speed: %d", pong.ballSpeed);
}

Game *gameInit(Gfx &gfx) {
  Game *game = (Game *)malloc(sizeof(Game));
  K_ENSURE(game != NULL);
  game->init(gfx);
  return game;
}

void gameQuit(Game *game) {
  game->quit();
  free(game);
}

void gameReset(Game *game) { game->reset(); }
void gameUpdate(Game *game, const Controls &controls, Gfx& gfx) {
  game->update(controls, gfx);
}
void gameRender(Game *game, Gfx &gfx) { game->render(gfx); }
void gameRenderImGui(Game *game) { game->renderImGui(); }

const GameAPI GAME_API = {
    gameInit, gameQuit, gameReset, gameUpdate, gameRender, gameRenderImGui,
};
