#pragma once

#include <cassert>
#include <random>
#include <vector>

#include "sdl.h"

enum class CellType {
  EMPTY, RED, GREEN, BLUE, YELLOW, SIZE
};

struct Point { int x; int y; };

struct Grid {
  std::vector<CellType> cells;
  int width;
  int height;

  std::random_device rd;
  std::minstd_rand gen;
  std::uniform_int_distribution<> rand;

  Grid(int width, int height): cells(width*height, CellType::RED),
                               width{width},
                               height{height},
                               gen(rd()),
                               rand(1, static_cast<int>(CellType::SIZE))
  {}

  CellType get(int x, int y) {
    assert(x >= 0 && x < width);
    assert(y >= 0 && y < height);
    return cells[y * width + x];
  }

  void randomize() {
    int size = width * height;
    cells.clear();
    while (size-- > 0) {
      cells.push_back(next_random_cell());
    }
  }

  CellType next_random_cell() {
    return static_cast<CellType>(rand(gen));
  }
};

enum class GameState {
  Main, GameOver, RotateLeft, RotateRight,
  PreHighlightMatchCells, HighlightMatchCells,
  RemoveMatchCells, FillHoles, CheckForCombos
};

struct Game {
  SDL_Scancode upkey;

  GameState state = GameState::Main;

  int selected_column = 0;
  Point selected_point = {1,3};
  int current_combo = 0;

  int width = 7;
  int height = 10;
  Grid grid;

  float delay;
  float delay_init;

  static constexpr int cell_width  = 50;
  static constexpr int cell_height = 50;
  static constexpr int margin = 2;

  std::vector<int> cells_in_rotation;

  Game() : grid(width, height) {
    grid.randomize();
  }

  void set_state(GameState s);
  void sdl_event(SDL_Event& e);
  void update(double dt);
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
