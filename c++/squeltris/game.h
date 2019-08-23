#include <algorithm>
#include <cassert>
#include <SDL2/SDL.h>

enum class Color {
  EMPTY, RED, GREEN, BLUE, YELLOW
};

struct Point { int x; int y; };

struct Grid {
  std::vector<Color> cells;
  int width;
  int height;

  Grid(int width, int height): cells(width*height, Color::RED),
                               width{width},
                               height{height} {}

  Color get(int x, int y) {
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

  void set_state(GameState s) {
    switch (state) {
      // do leave() stuff
    default:;
    }

    state = s;

    switch (s) {
      // do enter() stuff for that state
    default:;
    }
  }

  void sdl_event(SDL_Event& e) {
    switch (e.type) {
    case SDL_KEYDOWN:
      if (!e.key.repeat)
        upkey = e.key.keysym.scancode;
    }
  }

  void update() {
    switch (upkey) {
    case SDL_SCANCODE_UP   : move_up(); break;
    case SDL_SCANCODE_DOWN : move_down(); break;
    case SDL_SCANCODE_LEFT : move_left(); break;
    case SDL_SCANCODE_RIGHT: move_right(); break;
    case SDL_SCANCODE_Z    : rotate_left(); break;
    case SDL_SCANCODE_X    : rotate_right(); break;
    default:;
    }
    upkey = SDL_SCANCODE_UNKNOWN;
  }

  void render(SDL_Renderer *r) {
    SDL_SetRenderDrawColor(r, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(r);

    // Draw priest
    SDL_SetRenderDrawColor(r, 255, 255, 255, SDL_ALPHA_OPAQUE);
    Point center = {selected_point.x * cell_width,
                    selected_point.y * cell_height};
    SDL_RenderDrawLine(r, center.x-10, center.y   , center.x+10, center.y   );
    SDL_RenderDrawLine(r, center.x   , center.y-10, center.x   , center.y+10);

    // Draw grid
    for (auto x=0; x < width; ++x) {
      auto hole_found = false;
      for (auto y=0; y < height; ++y) {
        switch (grid.get(x,y)) {
        case Color::EMPTY: hole_found = true; continue;
        case Color::RED: SDL_SetRenderDrawColor(r, 200, 0, 0, SDL_ALPHA_OPAQUE); break;
        case Color::BLUE: SDL_SetRenderDrawColor(r, 0, 0, 200, SDL_ALPHA_OPAQUE); break;
        case Color::YELLOW: SDL_SetRenderDrawColor(r, 200, 200, 0, SDL_ALPHA_OPAQUE); break;
        case Color::GREEN: SDL_SetRenderDrawColor(r, 0, 200, 0, SDL_ALPHA_OPAQUE); break;
        }

        // Pixel coordinates of lower-left corner
        auto px = x * cell_width  + margin;
        auto py = y * cell_height + margin;
        // Pixel width and height of cell
        auto pw = cell_width  - margin*2;
        auto ph = cell_height - margin*2;

        auto xy = y * grid.width + x;
        auto off_x = 0;
        auto off_y = 0;

        SDL_Rect rect = {px + off_x, py + off_y, pw, ph};
        SDL_RenderFillRect(r, &rect);
      }
    }
  }

  void move_up() { selected_point.y = std::max(1, selected_point.y-1); }
  void move_down() { selected_point.y = std::min(height-1, selected_point.y+1); }
  void move_left() { selected_point.x = std::max(1, selected_point.x-1); }
  void move_right() { selected_point.x = std::min(width-1, selected_point.x+1); }

  void rotate_left() {
    auto [x,y] = selected_point;
    cells_in_rotation = {
       y    * width + x,
       y    * width + x-1,
      (y-1) * width + x-1,
      (y-1) * width + x
    };
    set_state(GameState::RotateLeft);
  }

  void rotate_cells_left() {
    if (cells_in_rotation.size()) {
      auto g = grid.cells;
      auto c = cells_in_rotation;
      auto bak = g[c[0]];
      g[c[0]] = g[c[3]];
      g[c[3]] = g[c[2]];
      g[c[2]] = g[c[1]];
      g[c[1]] = bak;
      c.clear();
    }
  }

  void rotate_right() {
    auto [x,y] = selected_point;
    cells_in_rotation = {
       y    * width + x,
       y    * width + x-1,
      (y-1) * width + x-1,
      (y-1) * width + x
    };
    set_state(GameState::RotateRight);
  }

  void rotate_cells_right() {
    if (cells_in_rotation.size()) {
      auto g = grid.cells;
      auto c = cells_in_rotation;
      auto bak = g[c[0]];
      g[c[0]] = g[c[1]];
      g[c[1]] = g[c[2]];
      g[c[2]] = g[c[3]];
      g[c[3]] = bak;
      c.clear();
    }
  }
};
