#include "app.h"

#include <chrono>

#include "game_api.h"

#include <unistd.h>

static const char *FONT_CHARS =
    " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',."
    "/~!@#$%^&*()_+{}|:\"<>?";

void Font::init(const App &app, const char *file, int width, int height,
                const char *charMapping, usize charMappingLength) {
  this->height = height;
  this->width = width;
  this->texture = SDL_CreateTextureFromSurface(app.renderer, SDL_LoadBMP(file));
  memset(charIndex, 0, sizeof(charIndex));
  for (usize i = 0; i < charMappingLength; ++i) {
    unsigned char c = charMapping[i];
    charIndex[c] = (u8)i;
  }
}

void Font::quit() { SDL_DestroyTexture(texture); }

void Font::draw(App &app, unsigned char c, int x, int y) {
  int i = charIndex[c];
  SDL_Rect src = {i * width, 0, width, height};
  SDL_Rect dst = {x, y, width, height};
  SDL_RenderCopy(app.renderer, texture, &src, &dst);
}

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void App::init() {
  const u32 logical_width = 800;
  const u32 logical_height = 450;
  const u32 scale = 1;

  const u32 window_width = logical_width * scale;
  const u32 window_height = logical_height * scale;

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER) != 0)
    sdl_die("Could not init SDL");

  window =
      SDL_CreateWindow("SDL demo", SDL_WINDOWPOS_UNDEFINED,
                       SDL_WINDOWPOS_UNDEFINED, window_width, window_height, 0);
  if (window == NULL)
    sdl_die("Could not create window");

  renderer = SDL_CreateRenderer(
      window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  if (renderer == NULL)
    sdl_die("Could not create renderer");

  SDL_RenderSetScale(renderer, scale, scale);
  SDL_RenderSetIntegerScale(renderer, SDL_TRUE);
  SDL_RenderSetLogicalSize(renderer, logical_width, logical_height);

  if (IMG_Init(IMG_INIT_PNG) == 0)
    sdl_die("Could not initialize IMG_PNG");

  font.init(*this, "data/font.bmp", 9, 16, FONT_CHARS, strlen(FONT_CHARS));

  memset(mouseButtonHeld, false, sizeof(mouseButtonHeld));
  memset(mouseButtonPreviousHeld, false, sizeof(mouseButtonPreviousHeld));
  lastMousePosition.x = 0;
  lastMousePosition.y = 0;
  memset(keyHeld, false, sizeof(keyHeld));
  memset(keyPreviousHeld, false, sizeof(keyPreviousHeld));
}

void App::quit() {
  font.quit();

  IMG_Quit();
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();
}

void App::run() {
  auto lastLoopTime = std::chrono::high_resolution_clock::now();
  u32 updateClockUs = 0;

  while (true) {
    swapInputState();

    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        return;
      case SDL_KEYDOWN:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          return;

#ifdef HOT_RELOAD
        if ((e.key.keysym.mod & KMOD_CTRL) && (e.key.keysym.sym == SDLK_r)) {
          hotReloadCallback();
          if (e.key.keysym.mod & KMOD_SHIFT)
            gameLib->api.reset(gameLib->state);
        }
#endif

        if (e.key.keysym.sym == SDLK_x || e.key.keysym.sym == SDLK_r ||
            e.key.keysym.sym == SDLK_e || e.key.keysym.sym == SDLK_m)
          updateKey(e.key.keysym.sym, false);
        break;

      case SDL_KEYUP:
        if (e.key.keysym.sym == SDLK_x || e.key.keysym.sym == SDLK_r ||
            e.key.keysym.sym == SDLK_e || e.key.keysym.sym == SDLK_m)
          updateKey(e.key.keysym.sym, true);
        break;

      case SDL_MOUSEMOTION:
        updateMousePosition(e.motion.x, e.motion.y);
        break;

      case SDL_MOUSEBUTTONDOWN:
      case SDL_MOUSEBUTTONUP:
        updateMouseButton(e.button.button, e.button.state);
        break;
      }
    }

    {
      auto loopTime = std::chrono::high_resolution_clock::now();
      std::chrono::duration<float, std::micro> loopDtUs =
        (loopTime - lastLoopTime);
      lastLoopTime = loopTime;
      updateClockUs += static_cast<u32>(loopDtUs.count());
      {
        const u32 usPerTick = 1000;
        const u32 ticks = updateClockUs / usPerTick;
        updateClockUs -= ticks * usPerTick;
        gameLib->api.update(gameLib->state, *this, ticks);
      }
    }

    const bool isFullscreen =
        SDL_GetWindowFlags(window) & SDL_WINDOW_FULLSCREEN_DESKTOP;

    if (isFullscreen) {
      if (button("Windowed", 16, 300)) {
        SDL_SetWindowFullscreen(window, 0);
      }
    } else {
      if (button("Fullscreen", 16, 300)) {
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
      }
    }

    gameLib->api.render(gameLib->state, *this);
    SDL_RenderPresent(renderer);

    //usleep(16000);
  }
}

u32 App::text(const char *s, int x, int y) {
  const u32 charWidth = font.width - 2;
  u32 pos = x;

  for (usize i = 0; s[i] != '\0'; ++i) {
    font.draw(*this, s[i], pos, y);
    pos += charWidth;
  }

  return pos - x + 1;
}

SDL_Rect App::boxedText(const char *s, u32 x, u32 y) {
  const u32 textWidth = text(s, x, y);

  const u32 textHeight = font.height;
  const u32 leftMargin = 2;
  const u32 topMargin = 2;
  const u32 rightMargin = 2;
  const u32 bottomMargin = 2;
  const int box_x = x - leftMargin;
  const int box_y = y - topMargin;
  const int box_w = leftMargin + textWidth + rightMargin;
  const int box_h = topMargin + textHeight + bottomMargin;
  drawRect(box_x, box_y, box_w, box_h);

  return SDL_Rect{box_x, box_y, box_w, box_h};
}

bool App::button(const char *s, u32 x, u32 y) {
  SDL_Rect box = boxedText(s, x, y);

  bool clicked = isMouseButtonHeld(SDL_BUTTON_LEFT) &&
                 SDL_PointInRect(&lastMousePosition, &box);

  return clicked;
}
