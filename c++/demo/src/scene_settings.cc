#include "scene.h"

void SceneSettings::enter(Gfx& gfx) { K_UNUSED(gfx); }
void SceneSettings::leave() {}
void SceneSettings::reset() {}

void SceneSettings::update(const Controls &controls) {
  K_UNUSED(controls);
}

void SceneSettings::render(Gfx &gfx) {
  clearScreen(ColorRGBA{30, 30, 30, 255});

  setDrawColor(ColorRGBA{255, 255, 255, 255});

  gfx.text("Settings", 200, 500);

  if (gfx.button("Lemmings", 200, 450)) {
    m_pGame->m_useLemmings = !m_pGame->m_useLemmings;
    printf("use lemmings=%d\n", m_pGame->m_useLemmings);
  }
  if (m_pGame->m_useLemmings)
    gfx.text("on", 280, 450);
  else
    gfx.text("off", 280, 450);

  if (gfx.button("Back", 350, 500)) {
    m_pGame->changeScene(SceneId::TitleScreen);
  }
}
