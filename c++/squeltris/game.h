#pragma once

#include <cassert>
#include <random>
#include <vector>

#include "sdl.h"

static constexpr int WINDOW_HEIGHT = 500;
static constexpr int WINDOW_WIDTH = 350;

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
                               rand(1, static_cast<int>(CellType::SIZE)-1)
  {}

  CellType get(int x, int y) {
    assert(x >= 0 && x < width);
    assert(y >= 0 && y < height);
    return cells[y * width + x];
  }

  void put(CellType c, int x, int y) {
    assert(x >= 0 && x < width);
    assert(y >= 0 && y < height);
    cells[y * width + x] = c;
  }

  void randomize() {
    int size = width * height;
    cells.clear();
    while (size-- > 0) {
      cells.push_back(next_random_cell());
    }
  }

  CellType next_random_cell() {
    // TODO: use a LFSR?
    // TODO: bagged random to ensure no run without some color?
    return static_cast<CellType>(rand(gen));
  }

  CellType push_down(int column, int row, CellType new_cell);
};

enum class GameState {
  Main, GameOver, RotateLeft, RotateRight,
  HighlightMatchCells, RemoveMatchCells, FillHoles,
  CheckForCombos
};

typedef std::vector<std::string> Pattern;

struct Match {
  Pattern& pattern;
  std::vector<int> cells;
};

enum struct PatternMatchType {
  NO_MATCH, MATCH, PASS,
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

  int frame = 0;

  static constexpr int cell_width  = 50;
  static constexpr int cell_height = 50;
  static constexpr int margin = 2;

  std::vector<int> cells_in_rotation;

  std::vector<Pattern> patterns;
  std::vector<Match> current_matches;
  std::vector<int> cells_in_match;
  std::vector<int> columns_with_holes;

  Game() : grid(width, height) {
    patterns = {
      {"000",
       ".0."},

      {"11",
       "1.",
       "1."},

      {".22",
       "22."},

      {"33",
       "33"},
    };

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

  void start_rotate(GameState next_state);
  void rotate_cells_left();
  void rotate_cells_right();

  void check_for_matches();
  std::vector<Match> check_all_matches();
  std::vector<int> match_pattern_at(Pattern& pattern, int x, int y);
  void remove_match_cells();
  void fill_holes();
};
