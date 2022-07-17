#include "app.h"

#include "game_api.h"

#include "imgui.h"
#include "imgui_impl_opengl2.h"
#include "imgui_impl_sdl.h"

// TODO:
// - Fix sprites
// - graph overlay with update/render/frame times

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void App::init() {
  logicalWidth = 800;
  logicalHeight = 450;

  const u32 windowWidth = logicalWidth;
  const u32 windowHeight = logicalHeight;

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER) != 0)
    sdl_die("Could not init SDL");

  // GL 2.1
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);

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

  controls.init();
  gfx.init(controls);

  initImGui();

  running = true;
}

void App::initImGui() {
  ImGui::CreateContext();
  ImGui::StyleColorsDark();

  ImGui_ImplSDL2_InitForOpenGL(window, glContext);
  ImGui_ImplOpenGL2_Init();
}

void App::quitImGui() {
  ImGui_ImplOpenGL2_Shutdown();
  ImGui_ImplSDL2_Shutdown();
  ImGui::DestroyContext();
}

void App::quit() {
  quitImGui();

  gfx.quit();
  controls.quit();

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
  controls.swapInputState();

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
        controls.updateKey(e.key.keysym.sym, false);
      break;

    case SDL_KEYUP:
      if (e.key.keysym.sym == SDLK_x || e.key.keysym.sym == SDLK_r ||
          e.key.keysym.sym == SDLK_e || e.key.keysym.sym == SDLK_m)
        controls.updateKey(e.key.keysym.sym, true);
      break;

    case SDL_MOUSEMOTION:
      controls.updateScreenMousePosition(e.motion.x, displayHeight - e.motion.y);
      controls.updateLogicalMousePosition(
          (float)e.motion.x * logicalWidth / displayWidth,
          (float)(displayHeight - e.motion.y) * logicalHeight / displayHeight);
      break;

    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
      controls.updateMouseButton(e.button.button, e.button.state);
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

    SDL_GL_GetDrawableSize(window, (int*)&displayWidth, (int*)&displayHeight);

    if (dt > 0) {
      s64 dtNanos = (dt * NANOS_PER_SECOND) / perfFrequency;
      lagAccumulator += dtNanos;

      // Stats for debugging
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
        gameLib->api.update(gameLib->state, controls);
      }
    }

    {
      glViewport(0, 0, displayWidth, displayHeight);

      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(0, logicalWidth, 0, logicalHeight, 0.0f, 1.0f);
    }

    gameLib->api.render(gameLib->state, gfx);
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

void App::drawImGui() {
  ImGuiIO &io = ImGui::GetIO();
  UNUSED(io);

  ImGui_ImplOpenGL2_NewFrame();
  ImGui_ImplSDL2_NewFrame();
  ImGui::NewFrame();

  ImGui::Text("This is some useful text.");

  ImGui::Render();
  ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
}
