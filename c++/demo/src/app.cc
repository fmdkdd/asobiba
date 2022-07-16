#include "app.h"

#include "game_api.h"

#include "imgui.h"
#include "imgui_impl_sdl.h"
#include "imgui_impl_opengl3.h"

static const char *FONT_CHARS =
    " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789`-=[]\\;',."
    "/~!@#$%^&*()_+{}|:\"<>?";

void Font::init(const App &app, const char *file, int width, int height,
                const char *charMapping, usize charMappingLength) {
  this->height = height;
  this->width = width;
  //this->texture = SDL_CreateTextureFromSurface(app.renderer, SDL_LoadBMP(file));
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
  //SDL_RenderCopy(app.renderer, texture, &src, &dst);
}

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void App::init() {
  const u32 logicalWidth = 800;
  const u32 logicalHeight = 450;
  const u32 scale = 1;

  const u32 windowWidth = logicalWidth * scale;
  const u32 windowHeight = logicalHeight * scale;

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER) != 0)
    sdl_die("Could not init SDL");

  // GL 3.0 + GLSL 130
  const char *glslVersion = "#version 130";
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, 0);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

  SDL_WindowFlags windowFlags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL);
  window = SDL_CreateWindow("SDL Demo", SDL_WINDOWPOS_UNDEFINED,
                            SDL_WINDOWPOS_UNDEFINED, windowWidth, windowHeight,
                            windowFlags);
  if (window == NULL)
    sdl_die("Could not create window");

  glContext = SDL_GL_CreateContext(window);

  if (glContext == NULL)
    sdl_die("Could not create GL context");

  if (SDL_GL_MakeCurrent(window, glContext) != 0)
    sdl_die("GL_MakeCurrent failed");

  if (IMG_Init(IMG_INIT_PNG) == 0)
    sdl_die("Could not initialize IMG_PNG");

  font.init(*this, "data/font.bmp", 9, 16, FONT_CHARS, strlen(FONT_CHARS));

  memset(mouseButtonHeld, false, sizeof(mouseButtonHeld));
  memset(mouseButtonPreviousHeld, false, sizeof(mouseButtonPreviousHeld));
  lastMousePosition.x = 0;
  lastMousePosition.y = 0;
  memset(keyHeld, false, sizeof(keyHeld));
  memset(keyPreviousHeld, false, sizeof(keyPreviousHeld));

  initImGui(glslVersion);

  running = true;
}

void App::initImGui(const char* glslVersion) {
  ImGui::CreateContext();
  ImGui::StyleColorsDark();

  ImGui_ImplSDL2_InitForOpenGL(window, glContext);
  ImGui_ImplOpenGL3_Init(glslVersion);
}

void App::quitImGui() {
  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplSDL2_Shutdown();
  ImGui::DestroyContext();
}

void App::quit() {
  quitImGui();

  font.quit();

  IMG_Quit();

  SDL_GL_DeleteContext(glContext);
  SDL_DestroyWindow(window);
  SDL_Quit();
}

u64 App::getHostRefreshRate() {
  // SDL_GetCurrentDisplayMode isn't precise: e.g. reports 60Hz when display
  // refreshes at 59.95.  We could probably measure this ourselves if such
  // a difference is problematic.

  int displayIndex = SDL_GetWindowDisplayIndex(window);
  if (displayIndex < 0) {
    SDL_Log("Failed get window display index, defaulting to 0.");
    displayIndex = 0;
  }
  SDL_DisplayMode current;
  if (SDL_GetCurrentDisplayMode(displayIndex, &current) != 0) {
    SDL_Log("Failed to query refresh rate, defaulting to 60Hz");
    current.refresh_rate = 60;
  }

  u64 hostRefreshRate = current.refresh_rate;
  return hostRefreshRate;
}

void App::updateInputs() {
  swapInputState();

  SDL_Event e;
  while (SDL_PollEvent(&e)) {

    ImGui_ImplSDL2_ProcessEvent(&e);

    switch (e.type) {
    case SDL_QUIT:
      running = false;
      return;

    case SDL_KEYDOWN:
      if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE) {
        running = false;
        return;
      }

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

    default:
      break;
    }
  }
}

void App::run() {
  // The goal of all this is to ensure the gameplay it not drastically impacted
  // by host refresh rate.  We favor frame presentation consistency over time
  // keeping.  That is, we may run the game at very slightly different speeds on
  // different monitors, in order to keep a consistent pattern of game updates
  // and renders.
  //
  // Just the keeping track of the elapsed frame time in an accumulator quickly
  // leads to an inconsistent pattern, because some frames may have a spike.  So
  // instead we assume a constant host refresh rate, and derive a constant
  // update rate from that.
  //
  // We use a fixed timestep of 480Hz, because it's small enough that we can fit
  // at least one update and see the results at rates higher than 60Hz.  And at
  // odd rates (144, 165), we are still making some progress so that we avoid
  // duplicate frames.

  static const s64 NANOS_PER_SECOND = 1000000000;

  u64 updateCounter = 0;
  const u64 updateRate = 480;
  const u64 targetRefreshRate = getHostRefreshRate();
  s64 lagAccumulator = 0;
  const s64 targetRefreshRateNanos = NANOS_PER_SECOND / targetRefreshRate;
  const s64 updateRateNanos = NANOS_PER_SECOND / updateRate;
  u64 lastLoopTick = 0;

#if 0
  u64 frameTimeHistory[128];
  u64 frameTimeHistoryIndex = 0;
#endif

  // Vsync is best to avoid tearing, but frame timings may be whack.
  // That's one of the reason we do not rely on elapsed time to trigger
  // updates.

  bool useVsync = true;
  if (useVsync) {
    if (SDL_GL_SetSwapInterval(1) != 0) {
      SDL_Log("Failed to enable vsync, continuing with vsync off");
      useVsync = false;
    }
  } else {
    if (SDL_GL_SetSwapInterval(0) != 0) {
      SDL_Log("Failed to disable vsync, continuing with vsync on");
      useVsync = true;
    }
  }

  // If Vsync is off, we must back off to keep the target framerate.  But
  // we still do not know exactly when the image is presented on the monitor.
  // Our frame timings with vsync off may be more precise, but this does not
  // necessarily mean that presented images are more regular.

  bool fixupFrametime = !useVsync;
  const u64 ticksPerFrame =
      targetRefreshRateNanos * SDL_GetPerformanceFrequency() / NANOS_PER_SECOND;

  const u64 perfFrequency = SDL_GetPerformanceFrequency();

  while (running) {
    u64 now = SDL_GetPerformanceCounter();

    // Wrap-around, unlikely
    if (now < lastLoopTick)
      lastLoopTick = 0;

    if (lastLoopTick == 0)
      lastLoopTick = now;

    s64 dt = now - lastLoopTick;
    lastLoopTick = now;

    if (dt > 0) {
      s64 dtNanos = (dt * NANOS_PER_SECOND) / perfFrequency;
      lagAccumulator += dtNanos;

      // Stats for debugging
      // TODO: graph overlay with update/render/frame times
#if 0
      {
        frameTimeHistory[frameTimeHistoryIndex] = dtNanos;
        frameTimeHistoryIndex++;
        if (frameTimeHistoryIndex == ARRAY_SIZE(frameTimeHistory))
          frameTimeHistoryIndex = 0;

        double avgDtNs = 0;
        for (usize i = 0; i < ARRAY_SIZE(frameTimeHistory); ++i)
          avgDtNs += frameTimeHistory[i];
        avgDtNs /= (double)ARRAY_SIZE(frameTimeHistory);

        printf("lag=%16zd dt=%16zd avg=%16.3f fps=%8.2f (%3zu)\n",
               lagAccumulator, dtNanos, avgDtNs, 1000000000.0 / avgDtNs,
               targetRefreshRate);
      }
#endif

      updateCounter += updateRate;

      // Update inputs only if we are sure there's at least one game update,
      // otherwise we may miss press/release events.
      if (updateCounter >= targetRefreshRate) {
        updateInputs();
      }

      while (updateCounter >= targetRefreshRate) {
        updateCounter -= targetRefreshRate;
        lagAccumulator -= updateRateNanos;
        gameLib->api.update(gameLib->state, *this);
      }
    }

    {
      int displayWidth, displayHeight;
      SDL_GL_GetDrawableSize(window, &displayWidth, &displayHeight);
      glViewport(0, 0, displayWidth, displayHeight);
    }

    gameLib->api.render(gameLib->state, *this);
    drawImGui();
    SDL_GL_SwapWindow(window);

    if (fixupFrametime) {
      u64 now = SDL_GetPerformanceCounter();
      u64 targetEndTick = lastLoopTick + ticksPerFrame;

      if (now < targetEndTick) {
        u64 toEnd = targetEndTick - now;
        u64 toEndNanos = toEnd * NANOS_PER_SECOND / perfFrequency;

        static const u64 minSleepDurationNanos = 1000000;
        if (toEndNanos > minSleepDurationNanos) {
          u32 sleepMillis = (toEndNanos - minSleepDurationNanos) / 1000000;
          SDL_Delay(sleepMillis);
        }

        while (SDL_GetPerformanceCounter() < targetEndTick) {
          ;
        }
      }
    }
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

void App::drawImGui() {
  ImGuiIO& io = ImGui::GetIO();

  ImGui_ImplOpenGL3_NewFrame();
  ImGui_ImplSDL2_NewFrame();
  ImGui::NewFrame();

  ImGui::Text("This is some useful text.");

  ImGui::Render();
  ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
}
