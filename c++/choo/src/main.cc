#include <glad/glad.h>

#include <GLFW/glfw3.h>

#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl3.h"

#include <chrono>
#include <cmath>
#include <cstdio>

#include "game.h"
#include "utils.h"
#include "vec.h"
#include "version.h"

// Mini Metro minimalist approach is good, but the RNG and lack of depth are
// not. OpenTTD cargo and city generation is a bit of a letdown, but micro is
// great. Factorio is genius, but what if we just wanted to play with trains?
//
// Cargo in TTD seem to appear out of thin air.  What if we went more for a
// Railroad Tycoon 3 simulation?  Cargo is generated by cities, and already
// transits between them, but at a slow pace.  You add trains to speed up trade
// routes, allowing more cargo to be created.  But cities work like recipes in
// Factorio: there's a clear ratio, you need e.g. 2 copper, 2 iron, 1 water to
// grow 1 pop.  Then pops also want to go to other cities.  Other thing that's
// great in Factorio is: you build everything, so there's a sense of
// accomplishment from going to nothing to covering the map with factories.
// In RT3, industries and cities exist, but you can compliment them, build new
// ones, and make them grow.
// With a slider at map generation, you could chose between no industries at
// all, and many.
// Industries should be dynamic though: construction prices should follow
// raw materials prices, wages should follow helping hands availability?

// TODO: stations? cargo?
// TODO: picking: hovering on station, cargo generators.. (is cursor in BB?)
// TODO: train state: in station, loading, ...
// TODO: placing track is trivial unless there is interesting terrain

static void glfwErrorCallback(int error, const char *description) {
  fprintf(stderr, "Glfw Error %d: %s\n", error, description);
}

bool gPreviousKeys[GLFW_KEY_LAST]; // 0=held, 1=up
bool gCurrentKeys[GLFW_KEY_LAST];

static void clearKeys() {
  memset(gPreviousKeys, true, ARRAY_SIZE(gPreviousKeys) * sizeof(bool));
  memset(gCurrentKeys, true, ARRAY_SIZE(gCurrentKeys) * sizeof(bool));
}

static void updateKeys(GLFWwindow *window) {
  const int usefulKeys[] = {GLFW_KEY_ESCAPE, GLFW_KEY_F, GLFW_KEY_D};

  for (size_t i = 0; i < ARRAY_SIZE(usefulKeys); ++i) {
    int key = usefulKeys[i];
    gPreviousKeys[key] = gCurrentKeys[key];
    gCurrentKeys[key] = glfwGetKey(window, key) == GLFW_RELEASE;
  }
}

static bool isKeyDown(int key) {
  ASSERT((usize)key < ARRAY_SIZE(gCurrentKeys));
  return !gCurrentKeys[key];
}

static bool isKeyUp(int key) {
  ASSERT((usize)key < ARRAY_SIZE(gCurrentKeys));
  return gCurrentKeys[key];
}

static bool wasKeyPressed(int key) {
  ASSERT((usize)key < ARRAY_SIZE(gCurrentKeys));
  return gPreviousKeys[key] && !gCurrentKeys[key];
}

static bool wasKeyReleased(int key) {
  ASSERT((usize)key < ARRAY_SIZE(gCurrentKeys));
  return !gPreviousKeys[key] && gCurrentKeys[key];
}

bool gPreviousMouseButtons[GLFW_MOUSE_BUTTON_LAST]; // 0=help, 1=up
bool gCurrentMouseButtons[GLFW_MOUSE_BUTTON_LAST];

static void clearMouseButtons() {
  memset(gPreviousMouseButtons, true,
         ARRAY_SIZE(gPreviousMouseButtons) * sizeof(bool));
  memset(gCurrentMouseButtons, true,
         ARRAY_SIZE(gCurrentMouseButtons) * sizeof(bool));
}

static void updateMouseButtons(GLFWwindow *window) {
  const int usefulButtons[] = {GLFW_MOUSE_BUTTON_LEFT, GLFW_MOUSE_BUTTON_RIGHT};

  for (size_t i = 0; i < ARRAY_SIZE(usefulButtons); ++i) {
    int button = usefulButtons[i];
    gPreviousMouseButtons[button] = gCurrentMouseButtons[button];
    gCurrentMouseButtons[button] =
        glfwGetMouseButton(window, button) == GLFW_RELEASE;
  }
}

static bool isMouseButtonDown(int button) {
  ASSERT((usize)button < ARRAY_SIZE(gCurrentMouseButtons));
  return !gCurrentMouseButtons[button];
}

static bool isMouseButtonUp(int button) {
  ASSERT((usize)button < ARRAY_SIZE(gCurrentMouseButtons));
  return gCurrentMouseButtons[button];
}

static bool wasMouseButtonPressed(int button) {
  ASSERT((usize)button < ARRAY_SIZE(gCurrentMouseButtons));
  return gPreviousMouseButtons[button] && !gCurrentMouseButtons[button];
}

static bool wasMouseButtonReleased(int button) {
  ASSERT((usize)button < ARRAY_SIZE(gCurrentMouseButtons));
  return !gPreviousMouseButtons[button] && gCurrentMouseButtons[button];
}

float gYScrollOffset;

static void glfwScrollCallback(GLFWwindow *window, double xoffset,
                               double yoffset) {
  UNUSED(window);
  UNUSED(xoffset);

  gYScrollOffset += (float)yoffset;
}

int main() {
  glfwSetErrorCallback(glfwErrorCallback);
  if (!glfwInit())
    return 1;

  // GL 3.0 + GLSL 130
  const char *glsl_version = "#version 130";
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 0);
  glfwWindowHint(GLFW_SAMPLES, 8);

  const char windowTitle[128] = {};
  snprintf(const_cast<char *>(windowTitle), ARRAY_SIZE(windowTitle),
           "CHOO %d.%d", CHOO_VERSION_MAJOR, CHOO_VERSION_MINOR);

  const int windowWidth = 800;
  const int windowHeight = 600;

  GLFWwindow *window =
      glfwCreateWindow(windowWidth, windowHeight, windowTitle, NULL, NULL);
  if (window == NULL)
    return 1;
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1); // Enable vsync

  if (gladLoadGL() == 0) {
    fprintf(stderr, "Failed to initialize OpenGL loader!\n");
    return 1;
  }

  ImGui::CreateContext();
  ImVec4 clear_color = ImVec4(77.0f/255.0f, 74.0f/255.0f, 90.0f/255.0f, 0.f);

  // Setup Platform/Renderer bindings
  ImGui_ImplGlfw_InitForOpenGL(window, true);
  ImGui_ImplOpenGL3_Init(glsl_version);

  glfwMakeContextCurrent(window);

  Game game;
  game.init();

  // {
  //   CargoGenerator &s1 = game.newCargoGenerator();
  //   CargoGenerator &s2 = game.newCargoGenerator();
  //   s2.pos = Vec2f(2.4f, -3.8f);
  //   s2.size = 1;
  // }

  enum EditState {
    IDLE,
    ADDING_TRACK_SEGMENT,
    ADDING_STATION,
  };

  enum AddTrackSegmentState {
    ADDING_BEGIN,
    ADDING_END,
  };

  static const char *EDIT_STATE_NAME[] = {"idle", "new track", "new station"};

  EditState editState = IDLE;
  AddTrackSegmentState addTrackSegmentState = ADDING_BEGIN;

  Vec2i trackSegmentBegin;

  Train *currentTrain = nullptr;
  Track *currentTrack = nullptr;

  float cameraZoom = 1.0f;
  static const float scrollZoomFactor = 1.3f;

  Vec2f cameraCenter = Vec2f(0.0f, 0.0f);
  static const float dragFactor = 1.0f;

  Vec2f cameraDragStart;
  bool isDraggingCamera = false;

  auto lastLoopTime = std::chrono::high_resolution_clock::now();
  u32 updateClockUs = 0;

  clearKeys();
  clearMouseButtons();

  glfwSetScrollCallback(window, glfwScrollCallback);

  // Main loop
  while (true) {
    glfwPollEvents();

    updateKeys(window);
    updateMouseButtons(window);

    if (wasKeyPressed(GLFW_KEY_ESCAPE))
      glfwSetWindowShouldClose(window, GLFW_TRUE);
    if (glfwWindowShouldClose(window))
      break;

    // Update mouse
    double mouseX;
    double mouseY;
    glfwGetCursorPos(window, &mouseX, &mouseY);

    const bool imguiCaptureMouse = ImGui::GetIO().WantCaptureMouse;

    // Update camera controls
    cameraZoom *= pow(scrollZoomFactor, gYScrollOffset);
    gYScrollOffset = 0.0f;

    if (isDraggingCamera) {
      cameraCenter.x +=
          (cameraDragStart.x - mouseX) * (dragFactor / cameraZoom);
      cameraCenter.y +=
          (mouseY - cameraDragStart.y) * (dragFactor / cameraZoom);

      cameraDragStart.x = mouseX;
      cameraDragStart.y = mouseY;

      if (wasMouseButtonReleased(GLFW_MOUSE_BUTTON_RIGHT))
        isDraggingCamera = false;
    } else {
      if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_RIGHT) &&
          !imguiCaptureMouse) {
        isDraggingCamera = true;
        cameraDragStart = Vec2f(mouseX, mouseY);
      }
    }

    // Update window and camera variables
    int displayWidth;
    int displayHeight;
    glfwGetFramebufferSize(window, &displayWidth, &displayHeight);
    const float halfWidth = (float)displayWidth / 2.0f;
    const float halfHeight = (float)displayHeight / 2.0f;

    const float cameraLeft = cameraCenter.x - halfWidth / cameraZoom;
    const float cameraRight = cameraCenter.x + halfWidth / cameraZoom;
    const float cameraBottom = cameraCenter.y - halfHeight / cameraZoom;
    const float cameraTop = cameraCenter.y + halfHeight / cameraZoom;

    const float mouseXWorldSpace =
        ((float)mouseX / (float)displayWidth) * (cameraRight - cameraLeft) +
        cameraLeft;
    const float mouseYWorldSpace =
        ((1.0f - ((float)mouseY / (float)displayHeight)) *
             (cameraTop - cameraBottom) +
         cameraBottom);
    const Vec2i mouseWorldSpace =
        Vec2i(mouseXWorldSpace + 0.5f, mouseYWorldSpace + 0.5f);

    // Update player interaction
    if (wasKeyPressed(GLFW_KEY_D)) {
      editState = ADDING_STATION;
    } else if (wasKeyPressed(GLFW_KEY_F)) {
      editState = ADDING_TRACK_SEGMENT;
      addTrackSegmentState = ADDING_BEGIN;
      // currentTrain = &game.newTrain();
      // currentTrain->track = currentTrack;
    }

    if (!imguiCaptureMouse) {
      if (editState == ADDING_TRACK_SEGMENT) {
        if (addTrackSegmentState == ADDING_BEGIN) {
          if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
            trackSegmentBegin = mouseWorldSpace;
            addTrackSegmentState = ADDING_END;
          }
        } else if (addTrackSegmentState == ADDING_END) {
          if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
            Vec2i trackSegmentEnd = mouseWorldSpace;
            game.addSegment(trackSegmentBegin, trackSegmentEnd);
            addTrackSegmentState = ADDING_BEGIN;
          }
        } else {
          UNREACHABLE();
        }
      } else if (editState == ADDING_STATION) {
        if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
          Station &s = game.newStation();
          s.pos = Vec2f(mouseXWorldSpace, mouseYWorldSpace);
        }
      }
    }

    // Update with fixed timestep
    auto loopTime = std::chrono::high_resolution_clock::now();
    std::chrono::duration<float, std::micro> loopDtUs =
        (loopTime - lastLoopTime);
    lastLoopTime = loopTime;
    updateClockUs += static_cast<u32>(loopDtUs.count());

    {
      const u32 timestepUs = 5000;
      while (updateClockUs >= timestepUs) {
        updateClockUs -= timestepUs;
        game.update();
      }
    }

    // Draw
    glViewport(0, 0, displayWidth, displayHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(cameraLeft, cameraRight, cameraBottom, cameraTop, 0.0f, 1.0f);
    glClearColor(clear_color.x, clear_color.y, clear_color.z, clear_color.w);
    glClear(GL_COLOR_BUFFER_BIT);

    game.render();

#if 0
    // Draw cursor
    {
      glColor4f(0.0f, 0.0f, 1.0f, 1.0f);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      glTranslatef(mouseXWorldSpace, mouseYWorldSpace, 0.0f);
      glScalef(0.02f, 0.02f, 1.0f);
      glRectf(-1.0f, -1.0f, 1.0f, 1.0f);
    }
#endif

    // Draw ImGui
    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplGlfw_NewFrame();
    ImGui::NewFrame();

    ImGui::Text("Frame time (ms): %f", loopDtUs.count() / 1000.0f);
    // ImGui::Text("viewport: %d %d", displayWidth, displayHeight);
    // ImGui::Text("camera space: %f %f %f %f", -halfWidth, halfWidth,
    // -halfHeight, halfHeight);
    ImGui::Text("mouse (screenspace): %f %f", mouseX, mouseY);
    ImGui::Text("mouse (worldspace): %f %f (%d %d)", mouseXWorldSpace,
                mouseYWorldSpace, mouseWorldSpace.x, mouseWorldSpace.y);
    // ImGui::SliderFloat("Direction", &train1.direction, 0.0f, 360.0f);

    ImGui::Text("Edit state: %s", EDIT_STATE_NAME[editState]);
    if (editState == ADDING_TRACK_SEGMENT) {
      ImGui::Text("Segment state: %d", addTrackSegmentState);
    }

    ImGui::Text("Points: %zu", game.network.pointCount);
    ImGui::Text("Edges: %zu", game.network.edgeCount);

    //   ImGui::Text("Train pos: %f", currentTrain->pos);
    //   ImGui::SliderFloat("Train speed", &currentTrain->speed, 0.0f, 0.02f);
    //   ImGui::Text("Track length: %f (%zu points)", currentTrack->length(),
    //   currentTrack->pointCount);
    // }

    // ImGui::Text("Generator 0 cargo: %d", game.cargoGenerators[0].cargoCount);

    ImGui::Render();
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());

    // Swap
    glfwSwapBuffers(window);
  }

  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplGlfw_Shutdown();
  ImGui::DestroyContext();

  glfwDestroyWindow(window);
  glfwTerminate();

  return 0;
}
