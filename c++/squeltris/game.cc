#include <algorithm>

#include "game.h"

void Game::set_state(GameState s) {
  // Leave transitions
  switch (state) {
  case GameState::RotateLeft:
    rotate_cells_left();
    break;
  case GameState::RotateRight:
    rotate_cells_right();
    break;

  default:;
  }

  state = s;

  // Enter transitions
  switch (s) {
  case GameState::RotateLeft:
  case GameState::RotateRight:
    delay = 0.07;
    delay_init = delay;
    break;

  default:;
  }
}

void Game::sdl_event(SDL_Event& e) {
  switch (state) {
  case GameState::Main:
    switch (e.type) {
    case SDL_KEYDOWN:
      if (!e.key.repeat)
        upkey = e.key.keysym.scancode;
    }
  default:;
  }
}

void Game::update(double dt) {
  switch (state) {
  case GameState::Main:
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
    break;

  case GameState::RotateLeft:
  case GameState::RotateRight:
    delay -= dt;
    if (delay < 0)
      set_state(GameState::Main);
    break;

  default:;
  }
}

void Game::render(SDLRenderer& r) {
  r.set_draw_color({0,0,0});
  r.clear();

  // Draw priest
  r.set_draw_color({255, 255, 255});
  Point center = {selected_point.x * cell_width,
                  selected_point.y * cell_height};
  r.draw_line(center.x-10, center.y   , center.x+10, center.y   );
  r.draw_line(center.x   , center.y-10, center.x   , center.y+10);

  // Draw grid
  std::vector<int> offset = {};
  float offset_value = 0;
  switch (state) {
  case GameState::RotateLeft:
  case GameState::RotateRight:
    offset = cells_in_rotation;
    offset_value = 1 - (delay / delay_init);
    break;
  default:;
  }
  bool rotateLeft = state == GameState::RotateLeft;

  for (auto x=0; x < width; ++x) {
    auto hole_found = false;
    for (auto y=0; y < height; ++y) {
      switch (grid.get(x,y)) {
      case CellType::EMPTY  : hole_found = true; continue;
      case CellType::RED    : r.set_draw_color({200, 0, 0}); break;
      case CellType::BLUE   : r.set_draw_color({0, 0, 200}); break;
      case CellType::YELLOW : r.set_draw_color({200, 200, 0}); break;
      case CellType::GREEN  : r.set_draw_color({0, 200, 0}); break;
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

      if (offset.size()) {
        auto index = std::distance(offset.begin(),
                                   std::find(offset.begin(), offset.end(), xy));
        if (rotateLeft) {
          switch (index) {
          case 0: off_y = -offset_value * cell_height; break;
          case 1: off_x =  offset_value * cell_width; break;
          case 2: off_y =  offset_value * cell_height; break;
          case 3: off_x = -offset_value * cell_width; break;
          }
        }
        else {
          switch (index) {
          case 0: off_x = -offset_value * cell_width; break;
          case 1: off_y = -offset_value * cell_height; break;
          case 2: off_x =  offset_value * cell_width; break;
          case 3: off_y =  offset_value * cell_height; break;
          }
        }
      }

      r.fill_rect(px + off_x, py + off_y, pw, ph);
    }
  }
}

void Game::move_up() { selected_point.y = std::max(1, selected_point.y-1); }
void Game::move_down() { selected_point.y = std::min(height-1, selected_point.y+1); }
void Game::move_left() { selected_point.x = std::max(1, selected_point.x-1); }
void Game::move_right() { selected_point.x = std::min(width-1, selected_point.x+1); }

void Game::rotate_left() {
  auto [x,y] = selected_point;
  cells_in_rotation = {
    y    * width + x,
    y    * width + x-1,
    (y-1) * width + x-1,
    (y-1) * width + x
  };
  set_state(GameState::RotateLeft);
}

void Game::rotate_cells_left() {
  if (cells_in_rotation.size()) {
    auto& g = grid.cells;
    auto& c = cells_in_rotation;
    auto bak = g[c[0]];
    g[c[0]] = g[c[3]];
    g[c[3]] = g[c[2]];
    g[c[2]] = g[c[1]];
    g[c[1]] = bak;
    c.clear();
  }
}

void Game::rotate_right() {
  auto [x,y] = selected_point;
  cells_in_rotation = {
    y    * width + x,
    y    * width + x-1,
    (y-1) * width + x-1,
    (y-1) * width + x
  };
  set_state(GameState::RotateRight);
}

void Game::rotate_cells_right() {
  if (cells_in_rotation.size()) {
    auto& g = grid.cells;
    auto& c = cells_in_rotation;
    auto bak = g[c[0]];
    g[c[0]] = g[c[1]];
    g[c[1]] = g[c[2]];
    g[c[2]] = g[c[3]];
    g[c[3]] = bak;
    c.clear();
  }
}
