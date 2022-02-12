#ifndef UI_H
#define UI_H

#include <GLFW/glfw3.h>

#include "vec.h"

struct Game;

struct UI {
  Game *game;

  enum EditState {
    IDLE,
    ADDING_TRACK_SEGMENT,
    ADDING_STATION,
  };

  enum AddTrackSegmentState {
    ADDING_BEGIN,
    ADDING_END,
    JOINING_END,
  };

  EditState editState;
  AddTrackSegmentState addTrackSegmentState;

  Vec2i trackSegmentBegin;
  PointId trackSegmentBeginPointId;
  Optional<PointId> closestPointOnNetwork;

  float cameraZoom;
  Vec2f cameraCenter;
  float cameraLeft;
  float cameraRight;
  float cameraBottom;
  float cameraTop;

  Vec2f cameraDragStart;
  bool isDraggingCamera;

  void init(Game *game);
  void updateInteraction(bool imguiCaptureMouse);
  void render() const;
  void renderImgui() const;

  bool previousKeys[GLFW_KEY_LAST]; // 0=held, 1=up
  bool currentKeys[GLFW_KEY_LAST];

  void updateKeys(GLFWwindow *window);
  bool isKeyDown(int key);
  bool isKeyUp(int key);
  bool wasKeyPressed(int key);
  bool wasKeyReleased(int key);

  bool previousMouseButtons[GLFW_MOUSE_BUTTON_LAST]; // 0=help, 1=up
  bool currentMouseButtons[GLFW_MOUSE_BUTTON_LAST];

  void updateMouseButtons(GLFWwindow *window);
  bool isMouseButtonDown(int button);
  bool isMouseButtonUp(int button);
  bool wasMouseButtonPressed(int button);
  bool wasMouseButtonReleased(int button);

  double mouseYScrollOffset;

  void updateScroll(double yOffset);

  double mouseX;
  double mouseY;

  void updateMouse(double x, double y);

  Vec2i mouseWorldSpace;

  void updateCameraControls(bool imguiCaptureMouse);
  void updateWindowCamera(int displayWidth, int displayHeight);
};

#endif
