cmake_minimum_required(VERSION 3.10)

# C++ standard, compile options

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)

project(demo VERSION 0.1)

option(ENABLE_ASSERT "Enable assert" ON)
option(HOT_RELOAD "Hot reloading of code" ON)

if (HOT_RELOAD)
  set(BUILD_SHARED_LIBS ON)
else()
  set(BUILD_SHARED_LIBS OFF)
endif()

# SDL2

add_subdirectory(deps/sdl2)
add_subdirectory(deps/sdl2_image)

# Game library

set(GAME_SRC_FILES
  src/app.h
  src/app.cc
  src/game.h
  src/game.cc
  )

if (HOT_RELOAD)
  add_library(game ${GAME_SRC_FILES})

  set_property(TARGET game PROPERTY POSITION_INDEPENDENT_CODE ON)

  if (ENABLE_ASSERT)
    target_compile_options(game PRIVATE -DENABLE_ASSERT)
  endif()

  if (MSVC)
    target_compile_options(game PRIVATE /W4 /WX)
  else()
    target_compile_options(game PRIVATE -Wall -Wextra -pedantic)
  endif()
endif()

# Binary

set(SOURCE_FILES
  src/app.h
  src/app.cc
  src/main.cc
  src/game_api.h
  src/utils.h
  )

if (HOT_RELOAD)
  add_executable(${PROJECT_NAME} ${SOURCE_FILES})
else()
  add_executable(${PROJECT_NAME} ${GAME_SRC_FILES} ${SOURCE_FILES})
endif()

if (MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE /W4 /WX)
else()
    target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -pedantic)
endif()

if (ENABLE_ASSERT)
  target_compile_options(${PROJECT_NAME} PRIVATE -DENABLE_ASSERT)
endif()

if (HOT_RELOAD)
  target_compile_definitions(${PROJECT_NAME} PRIVATE -DHOT_RELOAD)
  target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2 SDL2_image::SDL2_image)
else()
  target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2-static SDL2_image::SDL2_image-static)
endif()

add_custom_command(TARGET ${PROJECT_NAME} POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy_directory
                   ${CMAKE_SOURCE_DIR}/data $<TARGET_FILE_DIR:${PROJECT_NAME}>/data)
