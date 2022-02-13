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
  if (editState == ADDING_TRACK_SEGMENT) {
    auto pointRadius = config::previewPointRadius;
    auto pointResolution = config::previewPointResolution;
    auto segmentWidth = config::previewLineWidth;

    glColor4f(1.0f, 1.0f, 0.0f, 0.5f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    Vec2f candidatePoint = Vec2f(mouseWorldSpace.x, mouseWorldSpace.y);

    if (closestPointOnNetwork.hasValue) {
      Vec2i p = game->trackNetwork.getPoint(closestPointOnNetwork.get());
      candidatePoint = Vec2f(p.x, p.y);
      glColor4f(1.0f, 0.0f, 1.0f, 0.5f);
      drawCircle(candidatePoint, 1.5 * pointRadius, pointResolution);
    } else {
      drawCircle(candidatePoint, pointRadius, pointResolution);
    }

    if (addTrackSegmentState == ADDING_END ||
        addTrackSegmentState == JOINING_END) {
      glColor4f(1.0f, 1.0f, 0.0f, 0.5f);
      Vec2f begin = Vec2f(trackSegmentBegin.x, trackSegmentBegin.y);
      if (addTrackSegmentState == JOINING_END) {
        Vec2i p = game->trackNetwork.getPoint(trackSegmentBeginPointId);
        begin = Vec2f(p.x, p.y);
      }

      Vec2f end = candidatePoint;

      drawCircle(begin, pointRadius, pointResolution);

      Vec2f ps[] = {begin, end};
      drawLine(ps, 2, segmentWidth);
    }
  } else if (editState == ADDING_TRAIN_PICK_BEGIN) {
    auto pointRadius = config::previewPointRadius;
    auto pointResolution = config::previewPointResolution;

    if (closestPointOnNetwork.hasValue) {
      Vec2i p = game->trackNetwork.getPoint(closestPointOnNetwork.get());
      glColor4f(1.0f, 0.0f, 1.0f, 0.5f);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
      drawCircle(p, 1.5 * pointRadius, pointResolution);
    }

  } else if (editState == ADDING_TRAIN_PICK_END) {
    auto pointRadius = config::previewPointRadius;
    auto pointResolution = config::previewPointResolution;
    auto pathWidth = config::previewLineWidth;

    glColor4f(1.0f, 0.0f, 1.0f, 0.5f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    Vec2i beginPoint = game->trackNetwork.getPoint(addTrainBeginPoint);
    drawCircle(beginPoint, 1.5 * pointRadius, pointResolution);

    if (closestPointOnNetwork.hasValue) {
      PointId endPointId = closestPointOnNetwork.get();
      if (addTrainBeginPoint != endPointId) {
        Vec2i endPoint = game->trackNetwork.getPoint(endPointId);
        drawCircle(endPoint, 1.5 * pointRadius, pointResolution);
        if (addTrainShortestPath.pointCount > 0)
          drawPath(&game->trackNetwork, addTrainShortestPath, 1.5 * pathWidth);
      }
    }
  }
}

void UI::renderImgui() const {
  static const char *EDIT_STATE_NAME[] = {"idle", "new track", "new station",
                                          "new train (pick begin)",
                                          "new train (pick end)"};

  ImGui::Text("camera zoom: %f", cameraZoom);
  ImGui::Text("mouse (screenspace): %f %f", mouseX, mouseY);
  ImGui::Text("mouse (worldspace): %d %d", mouseWorldSpace.x,
              mouseWorldSpace.y);

  ImGui::Text("Edit state: %s", EDIT_STATE_NAME[editState]);
  if (editState == ADDING_TRACK_SEGMENT) {
    ImGui::Text("Segment state: %d", addTrackSegmentState);
  }

  ImGui::Text("Points: %zu", game->trackNetwork.pointCount);
  ImGui::Text("Edges: %zu", game->trackNetwork.edgeCount);

  // ImGui::SliderFloat("Direction", &train1.direction, 0.0f, 360.0f);
  if (game->trainCount > 0) {
    Train &currentTrain = game->trains[0];
    ImGui::Text("Train pos: %u", currentTrain.pathPosition);
    ImGui::SliderInt("Train speed", &currentTrain.speed, 0.0f, 10.0f);
    ImGui::Text("Track length: %u (%u points)", currentTrain.path.length(),
                currentTrain.path.pointCount);
  }

  // ImGui::Text("Generator 0 cargo: %d", game.cargoGenerators[0].cargoCount);
}

void UI::updateInteraction(bool imguiCaptureMouse) {
  if (wasKeyPressed(GLFW_KEY_D)) {
    editState = ADDING_TRACK_SEGMENT;
    addTrackSegmentState = ADDING_BEGIN;
  } else if (wasKeyPressed(GLFW_KEY_F)) {
    editState = ADDING_TRAIN_PICK_BEGIN;
  } else if (wasKeyPressed(GLFW_KEY_C)) {
    editState = ADDING_STATION;
  }

  if (imguiCaptureMouse)
    return;

  bool leftClick = wasMouseButtonPressed(GLFW_MOUSE_BUTTON_LEFT);

  if (editState == ADDING_TRACK_SEGMENT) {
    closestPointOnNetwork = game->trackNetwork.getClosestPoint(
        mouseWorldSpace, config::previewGrabTrackPointMaxDistance);

    if (addTrackSegmentState == ADDING_BEGIN) {
      if (leftClick) {
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
      if (leftClick) {
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
  } else if (editState == ADDING_TRAIN_PICK_BEGIN) {

    closestPointOnNetwork = game->trackNetwork.getClosestPoint(
        mouseWorldSpace, config::addTrainPickStationMaxDistance);

    if (leftClick && closestPointOnNetwork.hasValue) {
      addTrainBeginPoint = closestPointOnNetwork.get();
      editState = ADDING_TRAIN_PICK_END;
    }

  } else if (editState == ADDING_TRAIN_PICK_END) {

    closestPointOnNetwork = game->trackNetwork.getClosestPoint(
        mouseWorldSpace, config::addTrainPickStationMaxDistance);

    if (closestPointOnNetwork.hasValue) {
      PointId endPoint = closestPointOnNetwork.get();
      if (endPoint != addTrainBeginPoint) {
        game->trackNetwork.getShortestPath(addTrainBeginPoint, endPoint,
                                           &addTrainShortestPath);
      }

      if (leftClick) {
        Train &train = game->newTrain();
        train.setPath(addTrainShortestPath);
        editState = ADDING_TRAIN_PICK_BEGIN;
      }
    }

  } else if (editState == ADDING_STATION) {
    if (leftClick) {
      // Station &s = game->newStation();
      //  s.pos = mouseWorldSpace;
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
  const int usefulKeys[] = {GLFW_KEY_ESCAPE, GLFW_KEY_F, GLFW_KEY_D,
                            GLFW_KEY_C};

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
