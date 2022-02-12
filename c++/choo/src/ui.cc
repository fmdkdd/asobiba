#include "imgui.h"
#include <glad/glad.h>
#include <stdio.h>
#include <string.h>

#include "config.h"
#include "game.h"
#include "gfx.h"
#include "ui.h"

void UI::init(Game *game) {
  this->game = game;

  editState = IDLE;

  cameraZoom = 1.0f;
  cameraCenter = Vec2f(0, 0);
  isDraggingCamera = false;

  memset(previousKeys, true, ARRAY_SIZE(previousKeys) * sizeof(bool));
  memset(currentKeys, true, ARRAY_SIZE(currentKeys) * sizeof(bool));

  memset(previousMouseButtons, true,
         ARRAY_SIZE(previousMouseButtons) * sizeof(bool));
  memset(currentMouseButtons, true,
         ARRAY_SIZE(currentMouseButtons) * sizeof(bool));

  mouseYScrollOffset = 0;
  mouseX = 0;
  mouseY = 0;
}

void UI::render() const {
  auto clearColor = config::backgroundColor;

  glOrtho(cameraLeft, cameraRight, cameraBottom, cameraTop, 0.0f, 1.0f);
  glClearColor(clearColor.x, clearColor.y, clearColor.z, 0);
  glClear(GL_COLOR_BUFFER_BIT);

  if (editState == ADDING_TRACK_SEGMENT) {
    auto pointRadius = config::previewPointRadius;
    auto pointResolution = config::previewPointResolution;
    auto segmentWidth = config::previewLineWidth;

    glColor4f(1.0f, 1.0f, 0.0f, 0.5f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    Vec2f candidatePoint = Vec2f(mouseWorldSpace.x, mouseWorldSpace.y);

    if (closestPointOnNetwork.hasValue) {
      Network::Point p =
          game->trackNetwork.getPoint(closestPointOnNetwork.get());
      candidatePoint = Vec2f(p.x, p.y);
      glColor4f(1.0f, 0.0f, 1.0f, 1.0f);
      drawCircle(candidatePoint, 2 * pointRadius, pointResolution);
    } else {
      drawCircle(candidatePoint, pointRadius, pointResolution);
    }

    if (addTrackSegmentState == ADDING_END || addTrackSegmentState == JOINING_END) {
      glColor4f(1.0f, 1.0f, 0.0f, 0.5f);
      Vec2f begin = Vec2f(trackSegmentBegin.x, trackSegmentBegin.y);
      if (addTrackSegmentState == JOINING_END) {
        Network::Point p = game->trackNetwork.getPoint(trackSegmentBeginPointId);
        begin = Vec2f(p.x, p.y);
      }

      Vec2f end = candidatePoint;

      drawCircle(begin, pointRadius, pointResolution);

      Vec2f ps[] = {begin, end};
      drawLine(ps, 2, segmentWidth);
    }
  }
}

void UI::renderImgui() const {
  static const char *EDIT_STATE_NAME[] = {"idle", "new track", "new station"};

  ImGui::Text("camera zoom: %f", cameraZoom);
  ImGui::Text("mouse (screenspace): %f %f", mouseX, mouseY);
  ImGui::Text("mouse (worldspace): %d %d", mouseWorldSpace.x,
              mouseWorldSpace.y);
  // ImGui::SliderFloat("Direction", &train1.direction, 0.0f, 360.0f);

  ImGui::Text("Edit state: %s", EDIT_STATE_NAME[editState]);
  if (editState == ADDING_TRACK_SEGMENT) {
    ImGui::Text("Segment state: %d", addTrackSegmentState);
  }

  ImGui::Text("Points: %zu", game->trackNetwork.pointCount);
  ImGui::Text("Edges: %zu", game->trackNetwork.edgeCount);

  //   ImGui::Text("Train pos: %f", currentTrain->pos);
  //   ImGui::SliderFloat("Train speed", &currentTrain->speed, 0.0f, 0.02f);
  //   ImGui::Text("Track length: %f (%zu points)", currentTrack->length(),
  //   currentTrack->pointCount);
  // }

  // ImGui::Text("Generator 0 cargo: %d", game.cargoGenerators[0].cargoCount);
}

void UI::updateInteraction(bool imguiCaptureMouse) {
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
      closestPointOnNetwork = game->trackNetwork.getClosestPoint(
          mouseWorldSpace, config::previewGrabTrackPointMaxDistance);

      if (addTrackSegmentState == ADDING_BEGIN) {
        if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
          if (closestPointOnNetwork.hasValue) {
            trackSegmentBeginPointId = closestPointOnNetwork.get();
            addTrackSegmentState = JOINING_END;
          } else {
            trackSegmentBegin = mouseWorldSpace;
            addTrackSegmentState = ADDING_END;
          }
        }
      } else if (addTrackSegmentState == ADDING_END ||
                 addTrackSegmentState == JOINING_END) {
        if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
          u32 idBegin = addTrackSegmentState == ADDING_END
                            ? game->addPoint(trackSegmentBegin)
                            : trackSegmentBeginPointId;
          u32 idEnd = closestPointOnNetwork.hasValue
                          ? closestPointOnNetwork.get()
                          : game->addPoint(mouseWorldSpace);
          game->addSegment(idBegin, idEnd);

          addTrackSegmentState = ADDING_BEGIN;
        }
      } else {
        UNREACHABLE();
      }
    } else if (editState == ADDING_STATION) {
      if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT)) {
        // Station &s = game->newStation();
        //  s.pos = mouseWorldSpace;
      }
    }
  }
}

void UI::updateCameraControls(bool imguiCaptureMouse) {
  UNUSED(imguiCaptureMouse);

  static const float scrollZoomFactor = 1.3f;
  static const float dragFactor = 1.0f;

  if (mouseYScrollOffset != 0) {
    cameraZoom *= pow(scrollZoomFactor, mouseYScrollOffset);
    mouseYScrollOffset = 0;
  }

  if (isDraggingCamera) {
    cameraCenter.x += (cameraDragStart.x - mouseX) * (dragFactor / cameraZoom);
    cameraCenter.y += (mouseY - cameraDragStart.y) * (dragFactor / cameraZoom);

    cameraDragStart.x = mouseX;
    cameraDragStart.y = mouseY;

    if (wasMouseButtonReleased(GLFW_MOUSE_BUTTON_RIGHT))
      isDraggingCamera = false;
  } else {
    if (wasMouseButtonPressed(GLFW_MOUSE_BUTTON_RIGHT)) {
      isDraggingCamera = true;
      cameraDragStart = Vec2f(mouseX, mouseY);
    }
  }
}

void UI::updateWindowCamera(int displayWidth, int displayHeight) {
  const float halfWidth = (float)displayWidth / 2.0f;
  const float halfHeight = (float)displayHeight / 2.0f;

  cameraLeft = cameraCenter.x - halfWidth / cameraZoom;
  cameraRight = cameraCenter.x + halfWidth / cameraZoom;
  cameraBottom = cameraCenter.y - halfHeight / cameraZoom;
  cameraTop = cameraCenter.y + halfHeight / cameraZoom;

  float mouseXWorldSpace =
      ((float)mouseX / (float)displayWidth) * (cameraRight - cameraLeft) +
      cameraLeft;
  float mouseYWorldSpace = ((1.0f - ((float)mouseY / (float)displayHeight)) *
                                (cameraTop - cameraBottom) +
                            cameraBottom);

  mouseWorldSpace = Vec2i(mouseXWorldSpace + 0.5f, mouseYWorldSpace + 0.5f);
}

void UI::updateKeys(GLFWwindow *window) {
  const int usefulKeys[] = {GLFW_KEY_ESCAPE, GLFW_KEY_F, GLFW_KEY_D};

  for (size_t i = 0; i < ARRAY_SIZE(usefulKeys); ++i) {
    int key = usefulKeys[i];
    previousKeys[key] = currentKeys[key];
    currentKeys[key] = glfwGetKey(window, key) == GLFW_RELEASE;
  }
}

bool UI::isKeyDown(int key) {
  ASSERT((usize)key < ARRAY_SIZE(currentKeys));
  return !currentKeys[key];
}

bool UI::isKeyUp(int key) {
  ASSERT((usize)key < ARRAY_SIZE(currentKeys));
  return currentKeys[key];
}

bool UI::wasKeyPressed(int key) {
  ASSERT((usize)key < ARRAY_SIZE(currentKeys));
  return previousKeys[key] && !currentKeys[key];
}

bool UI::wasKeyReleased(int key) {
  ASSERT((usize)key < ARRAY_SIZE(currentKeys));
  return !previousKeys[key] && currentKeys[key];
}

void UI::updateMouseButtons(GLFWwindow *window) {
  const int usefulButtons[] = {GLFW_MOUSE_BUTTON_LEFT, GLFW_MOUSE_BUTTON_RIGHT};

  for (size_t i = 0; i < ARRAY_SIZE(usefulButtons); ++i) {
    int button = usefulButtons[i];
    previousMouseButtons[button] = currentMouseButtons[button];
    currentMouseButtons[button] =
        glfwGetMouseButton(window, button) == GLFW_RELEASE;
  }
}

bool UI::isMouseButtonDown(int button) {
  ASSERT((usize)button < ARRAY_SIZE(currentMouseButtons));
  return !currentMouseButtons[button];
}

bool UI::isMouseButtonUp(int button) {
  ASSERT((usize)button < ARRAY_SIZE(currentMouseButtons));
  return currentMouseButtons[button];
}

bool UI::wasMouseButtonPressed(int button) {
  ASSERT((usize)button < ARRAY_SIZE(currentMouseButtons));
  return previousMouseButtons[button] && !currentMouseButtons[button];
}

bool UI::wasMouseButtonReleased(int button) {
  ASSERT((usize)button < ARRAY_SIZE(currentMouseButtons));
  return !previousMouseButtons[button] && currentMouseButtons[button];
}

void UI::updateScroll(double yOffset) { mouseYScrollOffset = yOffset; }

void UI::updateMouse(double x, double y) {
  mouseX = x;
  mouseY = y;
}
