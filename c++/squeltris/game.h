#pragma once

#include <algorithm>
#include <cassert>
#include <vector>

#include "sdl.h"

enum class CellType {
  EMPTY, RED, GREEN, BLUE, YELLOW
};

struct Point { int x; int y; };

struct Grid {
  std::vector<CellType> cells;
  int width;
  int height;

  Grid(int width, int height): cells(width*height, CellType::RED),
                               width{width},
                               height{height} {}

  CellType get(int x, int y) {
    assert(x >= 0 && x < width);
    assert(y >= 0 && y < height);
    return cells[y * width + x];
  }
};

enum class GameState {
  Main, GameOver, RotateLeft, RotateRight
};

struct Game {
  SDL_Scancode upkey;

  GameState state;

  int selected_column = 0;
  Point selected_point = {1,3};
  int current_combo = 0;

  int width = 7;
  int height = 10;
  Grid grid;

  static constexpr int cell_width  = 50;
  static constexpr int cell_height = 50;
  static constexpr int margin = 2;

  std::vector<int> cells_in_rotation;

  Game() : grid(width, height) {}

  void set_state(GameState s);
  void sdl_event(SDL_Event& e);
  void update();
  void render(SDLRenderer& r);

  void move_up();
  void move_down();
  void move_left();
  void move_right();

  void rotate_left();
  void rotate_cells_left();

  void rotate_right();
  void rotate_cells_right();
};
