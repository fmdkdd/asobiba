#ifndef APP_H
#define APP_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_opengl.h>

#include "controls.h"
#include "gfx.h"
#include "utils.h"

struct GameLib;

struct StatHistory {
  float m_Values[1024];
  usize m_Count;
  usize m_Index;

  float m_Max;
  float m_Min;

  void Init();

  void Push(float value);
  float GetAverage() const;
};

struct App {
  u32 m_LogicalWidth;
  u32 m_LogicalHeight;

  u32 m_DisplayWidth;
  u32 m_DisplayHeight;

  bool m_IsVSyncOn;
  u64 m_HostDisplayRefreshRate;

  float m_TimeScale;

  StatHistory m_FrameTimeHistory;
  StatHistory m_UpdateTimeHistory;
  StatHistory m_RenderTimeHistory;

  SDL_Window *m_Window;
  SDL_GLContext m_GLContext;

  Controls m_Controls;
  Gfx m_Gfx;

  bool m_IsRunning;

  GameLib *m_GameLib;

  void (*m_HotReloadCallback)();

  void Init();
  void Run();
  void Quit();

  void InitImGui();
  void QuitImGui();
  void DrawImGui();

  u64 GetTicksPerFrame();
  u64 GetHostRefreshRate();
  void SetVSync(bool useVSync);

  void UpdateInputs();
};

#endif
