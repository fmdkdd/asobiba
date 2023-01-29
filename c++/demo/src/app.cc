#include "app.h"

#include "game_api.h"

#include "imgui.h"
#include "imgui_impl_opengl2.h"
#include "imgui_impl_sdl.h"

// TODO: title -> new game -> death -> restart, menus with animations
// TODO: settings menu, vsync, window size, fullscreen

// TODO: neonwise line that follows cursor, but with 'weight' and elasticity
// TODO: fullscreen aspect ratio keep

static void sdl_die(const char *msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void App::Init() {
  m_LogicalWidth = 800;
  m_LogicalHeight = 600;

  const u32 windowWidth = m_LogicalWidth;
  const u32 windowHeight = m_LogicalHeight;

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER) != 0)
    sdl_die("Could not init SDL");

  // GL 2.1
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);

  SDL_WindowFlags windowFlags = (SDL_WindowFlags)(SDL_WINDOW_OPENGL);
  m_Window = SDL_CreateWindow("SDL Demo", SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED, windowWidth,
                              windowHeight, windowFlags);
  if (m_Window == NULL)
    sdl_die("Could not create window");

  m_GLContext = SDL_GL_CreateContext(m_Window);

  if (m_GLContext == NULL)
    sdl_die("Could not create GL context");

  if (SDL_GL_MakeCurrent(m_Window, m_GLContext) != 0)
    sdl_die("GL_MakeCurrent failed");

  if (IMG_Init(IMG_INIT_PNG) == 0)
    sdl_die("Could not initialize IMG_PNG");

  m_Controls.init();
  m_Gfx.init(m_Controls);

  InitImGui();

  m_IsRunning = true;
}

void App::InitImGui() {
  ImGui::CreateContext();
  ImGui::StyleColorsDark();

  ImGui_ImplSDL2_InitForOpenGL(m_Window, m_GLContext);
  ImGui_ImplOpenGL2_Init();

  m_FrameTimeHistory.Init();
  m_UpdateTimeHistory.Init();
  m_RenderTimeHistory.Init();
}

void App::QuitImGui() {
  ImGui_ImplOpenGL2_Shutdown();
  ImGui_ImplSDL2_Shutdown();
  ImGui::DestroyContext();
}

void App::Quit() {
  QuitImGui();

  m_Gfx.quit();
  m_Controls.quit();

  IMG_Quit();

  SDL_GL_DeleteContext(m_GLContext);
  SDL_DestroyWindow(m_Window);
  SDL_Quit();
}

u64 App::GetHostRefreshRate() {
  // SDL_GetCurrentDisplayMode isn't precise: e.g. reports 60Hz when display
  // refreshes at 59.95.  We could probably measure this ourselves if such
  // a difference is problematic.

  int displayIndex = SDL_GetWindowDisplayIndex(m_Window);
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

void App::SetVSync(bool useVSync) {
  if (useVSync) {
    if (SDL_GL_SetSwapInterval(1) != 0) {
      SDL_Log("Failed to enable vsync, continuing with vsync off");
      useVSync = false;
    }
  } else {
    if (SDL_GL_SetSwapInterval(0) != 0) {
      SDL_Log("Failed to disable vsync, continuing with vsync on");
      useVSync = true;
    }
  }

  m_IsVSyncOn = useVSync;
}

void App::UpdateInputs() {
  SDL_Event e;
  while (SDL_PollEvent(&e)) {

    ImGui_ImplSDL2_ProcessEvent(&e);

    switch (e.type) {
    case SDL_QUIT:
      m_IsRunning = false;
      return;

    case SDL_KEYDOWN:
      if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE) {
        m_IsRunning = false;
        return;
      }

#ifdef HOT_RELOAD
      if ((e.key.keysym.mod & KMOD_CTRL) && (e.key.keysym.sym == SDLK_r)) {
        m_HotReloadCallback();
        if (e.key.keysym.mod & KMOD_SHIFT)
          m_GameLib->api.reset(m_GameLib->state);
      } else
#endif
        m_Controls.onKeyDown(e.key.keysym.sym);
      break;

    case SDL_KEYUP:
      m_Controls.onKeyUp(e.key.keysym.sym);
      break;

    case SDL_MOUSEMOTION:
      m_Controls.updateScreenMousePosition(e.motion.x,
                                           m_DisplayHeight - e.motion.y);
      m_Controls.updateLogicalMousePosition(
          (float)e.motion.x * m_LogicalWidth / m_DisplayWidth,
          (float)(m_DisplayHeight - e.motion.y) * m_LogicalHeight /
              m_DisplayHeight);
      break;

    case SDL_MOUSEBUTTONDOWN:
      m_Controls.onMouseButtonDown(e.button.button);
      break;

    case SDL_MOUSEBUTTONUP:
      m_Controls.onMouseButtonUp(e.button.button);
      break;

    default:
      break;
    }
  }
}

u64 App::GetTicksPerFrame() {
  static const u64 NANOS_PER_SECOND = 1'000'000'000;

  const u64 targetRefreshRate = m_HostDisplayRefreshRate;
  const u64 targetRefreshRateNanos = NANOS_PER_SECOND / targetRefreshRate;
  const u64 ticksPerFrame =
      targetRefreshRateNanos * SDL_GetPerformanceFrequency() / NANOS_PER_SECOND;

  return ticksPerFrame;
}

void App::Run() {
  static const u64 NANOS_PER_SECOND = 1'000'000'000;

  const u64 updateRate = 60;
  const s64 updateRateNanos = NANOS_PER_SECOND / updateRate;
  s64 lagAccumulator = 0;
  u64 lastLoopTick = 0;

  const u64 perfFrequency = SDL_GetPerformanceFrequency();

  SetVSync(true);
  m_HostDisplayRefreshRate = GetHostRefreshRate();
  m_TimeScale = 1.0f;

  while (m_IsRunning) {
    u64 now = SDL_GetPerformanceCounter();

    // Wrap-around, unlikely
    if (now < lastLoopTick)
      lastLoopTick = 0;

    if (lastLoopTick == 0)
      lastLoopTick = now;

    s64 ticksSinceLastLoop = now - lastLoopTick;
    lastLoopTick = now;

    SDL_GL_GetDrawableSize(m_Window, (int *)&m_DisplayWidth,
                           (int *)&m_DisplayHeight);

    UpdateInputs();

    if (ticksSinceLastLoop > 0) {
      s64 nanosSinceLastLoop =
          (ticksSinceLastLoop * NANOS_PER_SECOND) / perfFrequency;
      lagAccumulator += nanosSinceLastLoop * m_TimeScale;

      // Stats for debugging
      m_FrameTimeHistory.Push((double)nanosSinceLastLoop / 1'000'000);

      u64 startUpdate = SDL_GetPerformanceCounter();

      while (lagAccumulator >= updateRateNanos) {
        lagAccumulator -= updateRateNanos;
        m_GameLib->api.update(m_GameLib->state, m_Controls);
        m_Controls.swapGameInputState();
      }

      u64 endUpdate = SDL_GetPerformanceCounter();
      u64 updateTime =
          (endUpdate - startUpdate) * NANOS_PER_SECOND / perfFrequency;
      m_UpdateTimeHistory.Push((double)updateTime / 1'000'000);
    }

    {
      glViewport(0, 0, m_DisplayWidth, m_DisplayHeight);

      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);

      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(0, m_LogicalWidth, 0, m_LogicalHeight, 0.0f, 1.0f);
    }

    {
      u64 startRender = SDL_GetPerformanceCounter();
      m_GameLib->api.render(m_GameLib->state, m_Gfx);
      m_Controls.swapRenderInputState();
      u64 endRender = SDL_GetPerformanceCounter();
      u64 renderTime =
          (endRender - startRender) * NANOS_PER_SECOND / perfFrequency;
      m_RenderTimeHistory.Push((double)renderTime / 1'000'000);
    }

    DrawImGui();
    SDL_GL_SwapWindow(m_Window);

    if (!m_IsVSyncOn) {
      u64 now = SDL_GetPerformanceCounter();
      u64 targetEndTick = lastLoopTick + GetTicksPerFrame();

      if (now < targetEndTick) {
        u64 toEnd = targetEndTick - now;
        u64 toEndNanos = toEnd * NANOS_PER_SECOND / perfFrequency;

        static const u64 minSleepDurationNanos = 1'000'000;
        if (toEndNanos > minSleepDurationNanos) {
          u32 sleepMillis = (toEndNanos - minSleepDurationNanos) / 1'000'000;
          SDL_Delay(sleepMillis);
        }

        while (SDL_GetPerformanceCounter() < targetEndTick) {
          ;
        }
      }
    }
  }
}

void App::DrawImGui() {
  ImGuiIO &io = ImGui::GetIO();
  K_UNUSED(io);

  ImGui_ImplOpenGL2_NewFrame();
  ImGui_ImplSDL2_NewFrame();
  ImGui::NewFrame();

  ImGui::Begin("Stats");

  ImGui::PlotLines("Frametime", m_FrameTimeHistory.m_Values,
                   m_FrameTimeHistory.m_Count, m_FrameTimeHistory.m_Index, NULL,
                   0, 18, ImVec2(0, 60));

  ImGui::Text("Frametime avg=%.3f min=%.3f max=%.3f",
              m_FrameTimeHistory.GetAverage(), m_FrameTimeHistory.m_Min,
              m_FrameTimeHistory.m_Max);

  ImGui::PlotLines("Update time", m_UpdateTimeHistory.m_Values,
                   m_UpdateTimeHistory.m_Count, m_UpdateTimeHistory.m_Index,
                   NULL, 0, 10, ImVec2(0, 20));
  ImGui::PlotLines("Render time", m_RenderTimeHistory.m_Values,
                   m_RenderTimeHistory.m_Count, m_RenderTimeHistory.m_Index,
                   NULL, 0, 10, ImVec2(0, 20));

  bool vsync = m_IsVSyncOn;
  if (ImGui::Checkbox("VSync", &vsync)) {
    SetVSync(vsync);
  }

  ImGui::SliderInt("Host framerate", (int *)&m_HostDisplayRefreshRate, 10, 200);

  ImGui::SliderFloat("Timescale", &m_TimeScale, 0.1f, 20.0f);

  ImGui::End();

  m_GameLib->api.renderImGui(m_GameLib->state);

  ImGui::Render();
  ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());
}

void StatHistory::Init() {
  m_Count = 0;
  m_Index = 0;
  m_Min = 0;
  m_Max = 0;
}

void StatHistory::Push(float value) {
  m_Values[m_Index] = value;
  m_Index++;
  if (m_Index == K_ARRAY_SIZE(m_Values))
    m_Index = 0;
  if (m_Count < K_ARRAY_SIZE(m_Values))
    m_Count++;

  if (value > m_Max)
    m_Max = value;
  if (value < m_Min)
    m_Min = value;
}

float StatHistory::GetAverage() const {
  double avg = 0;
  for (usize i = 0; i < m_Count; ++i)
    avg += m_Values[i];
  avg /= (double)m_Count;
  return (float)avg;
}
