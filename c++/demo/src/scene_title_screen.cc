#include "scene.h"

static void toState(SceneTitleScreen &s, u32 state, u32 delay) {
  s.m_state = state;
  s.m_timer = delay;
}

void SceneTitleScreen::enter(Gfx& gfx) { K_UNUSED(gfx); }
void SceneTitleScreen::leave() {}

void SceneTitleScreen::reset() {
  m_state = 0;
  m_timer = 20;
  m_buttonsOffsetX = 0;
  m_buttonsEnabled = false;
}

// Want to write instead:
/*

fn doTitleScreenIntro():
   x = 0
   wait(20)
   while x < 200:
         x+= 2
         wait(20)
   buttons.enabled = true

Idea 1: use objects with state?
Idea 2: compile that to C?
 */

void SceneTitleScreen::update(const Controls &controls) {
  K_UNUSED(controls);

  if (m_timer > 0) {
    m_timer -= 1;
    return;
  }

  switch (m_state) {
  case 0:
    m_buttonsOffsetX += 2;
    if (m_buttonsOffsetX < 200)
      m_timer = 1;
    else
      toState(*this, 1, 60);
    break;

  case 1:
    m_buttonsEnabled = true;
    break;
  }
}

void SceneTitleScreen::render(Gfx &gfx) {
  clearScreen(ColorRGBA{30, 30, 30, 255});

  setDrawColor(ColorRGBA{255, 255, 255, 255});

  gfx.text("Title", 200, 500);

  u32 x = 200 + m_buttonsOffsetX;

  if (gfx.button("Play", x, 450, m_buttonsEnabled)) {
    m_pGame->changeScene(SceneId::Demo);
  }

  if (gfx.button("Settings", x, 400, m_buttonsEnabled)) {
    m_pGame->changeScene(SceneId::Settings);
  }
}
