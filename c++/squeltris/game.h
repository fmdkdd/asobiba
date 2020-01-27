#pragma once

#include <array>
#include <cassert>
#include <random>
#include <vector>

#include "sdl.h"

constexpr int WINDOW_HEIGHT = 224;
constexpr int WINDOW_WIDTH = 256;

enum class CellType {
  EMPTY, RED, GREEN, BLUE, YELLOW, STONE, SIZE
};

struct Point { int x; int y; };

struct Cell {
  CellType type;
  int counter;
};

struct Grid {
  std::vector<Cell> cells;
  int width;
  int height;

  std::random_device rd;
  std::minstd_rand gen;
  std::uniform_int_distribution<> rand;

  Grid(int width, int height): cells(width*height, {CellType::EMPTY, 0}),
                               width{width},
                               height{height},
                               gen(rd()),
                               rand(1, static_cast<int>(CellType::SIZE)-2)
  {}

  Cell get(int x, int y) {
    assert(x >= 0 && x < width);
    assert(y >= 0 && y < height);
    return cells[y * width + x];
  }

  void put(Cell c, int x, int y) {
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

  Cell next_random_cell() {
    // TODO: use a LFSR?
    // TODO: bagged random to ensure no run without some color?
    return {static_cast<CellType>(rand(gen)), 0};
  }

  Cell push_down(int column, int row, Cell new_cell);
};

enum class GameState {
  Main, GameOver, RotateLeft, RotateRight,
  HighlightMatchCells, RemoveMatchCells, FillHoles,
  CheckForCombos
};

struct Pattern {
  std::vector<std::string> *lines;
  CellType color;
  int counter = 0;
};

static std::array<std::vector<std::string>,6> all_patterns = {{
  {"XXX",
   ".X."},

  {"XX",
   "X.",
   "X."},

  {"XX",
   ".X",
   ".X"},

  {".XX",
   "XX."},

  {"XX.",
   ".XX"},

  {"XX",
   "XX"},
}};

constexpr int pattern_cell_height = 8;
constexpr int pattern_cell_width = 8;

struct Match {
  Pattern& pattern;
  std::vector<int> cells;
};

enum struct PatternMatchType {
  NO_MATCH, MATCH, PASS,
};

enum PlayerInput
{
 CLEAR,
 UP, RIGHT, DOWN, LEFT,
 ROTATE_LEFT, ROTATE_RIGHT
};

struct Game {
  PlayerInput playerInput;

  GameState state = GameState::Main;

  int selected_column = 0;
  Point selected_point = {3,5};
  int current_combo = 0;

  int width = 7;
  int height = 10;
  Grid grid;

  float delay;
  float delay_init;

  int frame = 0;

  int timer = 0;

  static constexpr int player_size  = 4;
  static constexpr int cell_width  = 18;
  static constexpr int cell_height = 18;
  static constexpr int margin = 1;

  std::vector<int> cells_in_rotation;

  std::vector<Pattern> patterns;
  std::vector<Match> current_matches;
  std::vector<int> cells_in_match;
  std::vector<int> columns_with_holes;

  Game() : grid(width, height) {
    patterns.push_back({&all_patterns[0], CellType::RED});
    patterns.push_back({&all_patterns[1], CellType::GREEN});
    patterns.push_back({&all_patterns[2], CellType::BLUE});
    patterns.push_back({&all_patterns[3], CellType::YELLOW});

    for (int c=0; c < width; ++c)
      columns_with_holes.push_back(c);
    set_state(GameState::FillHoles);

    timer = 1000;
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
  void transform_stones();
  void change_patterns();
};
