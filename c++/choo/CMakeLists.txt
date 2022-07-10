cmake_minimum_required(VERSION 3.10)

# C++ standard, compile options

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

project(choo VERSION 0.1)

set(BUILD_SHARED_LIBS OFF)

# glad

set(GLAD_API "gl=3.0")

add_subdirectory(deps/glad)

# GLFW

set(GLFW_BUILD_EXAMPLES OFF)
set(GLFW_BUILD_TESTS OFF)
set(GLFW_BUILD_DOCS OFF)

add_subdirectory(deps/glfw)

# ImGui

add_library(imgui
  deps/imgui/imgui.cpp
  deps/imgui/imgui_draw.cpp
  deps/imgui/imgui_tables.cpp
  deps/imgui/imgui_widgets.cpp
  deps/imgui/backends/imgui_impl_opengl3.cpp
  deps/imgui/backends/imgui_impl_glfw.cpp)

target_include_directories(imgui
  PUBLIC
  ${PROJECT_SOURCE_DIR}/deps/imgui
  ${PROJECT_SOURCE_DIR}/deps/imgui/backends)

target_link_libraries(imgui PRIVATE glad glfw)
target_compile_options(imgui PRIVATE -DIMGUI_IMPL_OPENGL_LOADER_GLAD)

# Binary

option(ENABLE_ASSERT "Enable assert" ON)

add_executable(${PROJECT_NAME}
  src/version.h.in
  src/main.cc
  src/config.h
  src/factory.h
  src/factory.cc
  src/game.h
  src/game.cc
  src/gfx.h
  src/gfx.cc
  src/network.h
  src/network.cc
  src/station.h
  src/station.cc
  src/train.h
  src/train.cc
  src/ui.h
  src/ui.cc
  src/utils.h
  src/vec.h)

if (MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE /W4 /WX)
else()
    target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -pedantic)
endif()

if (ENABLE_ASSERT)
  target_compile_options(${PROJECT_NAME} PRIVATE -DENABLE_ASSERT)
endif()

configure_file(src/version.h.in include/version.h)

target_include_directories(${PROJECT_NAME} PRIVATE
  ${PROJECT_BINARY_DIR}/include)

target_link_libraries(${PROJECT_NAME} PUBLIC glad glfw imgui)
