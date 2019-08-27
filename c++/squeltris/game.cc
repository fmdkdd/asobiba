#include <algorithm>

#include "game.h"

static void unreachable() {
  assert(false);
}

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

  case GameState::CheckForCombos:
    check_for_matches();
    break;

  case GameState::PreHighlightMatchCells:
    delay = 0.2;
    break;

  case GameState::HighlightMatchCells:
    delay = 0.3;
    break;

  case GameState::RemoveMatchCells:
    remove_match_cells();
    delay = 0.1;
    break;

  case GameState::FillHoles:
    delay = 0.05;
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
    case SDL_SCANCODE_I:
    case SDL_SCANCODE_UP   : move_up(); break;
    case SDL_SCANCODE_K:
    case SDL_SCANCODE_DOWN : move_down(); break;
    case SDL_SCANCODE_J:
    case SDL_SCANCODE_LEFT : move_left(); break;
    case SDL_SCANCODE_L:
    case SDL_SCANCODE_RIGHT: move_right(); break;
    case SDL_SCANCODE_Z    : start_rotate(GameState::RotateLeft); break;
    case SDL_SCANCODE_X    : start_rotate(GameState::RotateRight); break;
    default:;
    }
    upkey = SDL_SCANCODE_UNKNOWN;
    break;

  case GameState::RotateLeft:
  case GameState::RotateRight:
    delay -= dt;
    if (delay < 0)
      set_state(GameState::CheckForCombos);
    break;

  case GameState::CheckForCombos:
    set_state(GameState::Main);
    break;

  case GameState::PreHighlightMatchCells:
    delay -= dt;
    if (delay < 0)
      set_state(GameState::HighlightMatchCells);
    break;

  case GameState::HighlightMatchCells:
    delay -= dt;
    if (delay < 0)
      set_state(GameState::RemoveMatchCells);
    break;

  case GameState::RemoveMatchCells:
    delay -= dt;
    if (delay < 0)
      set_state(GameState::FillHoles);
    break;

  case GameState::FillHoles:
    delay -= dt;
    if (delay < 0) {
      fill_holes();
      // This updates the number of columns with holes left
      // Continue in FillHoles state if there are holes
      if (!columns_with_holes.empty()) {
        delay = delay_init;
      } else {
        // Otherwise go to CheckForCombos
        set_state(GameState::CheckForCombos);
      }
    }
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
  std::vector<int> offset;
  std::vector<int> downward;
  float offset_value = 0;
  switch (state) {
  case GameState::RotateLeft:
  case GameState::RotateRight:
    offset = cells_in_rotation;
    offset_value = 1 - (delay / delay_init);
    break;

  case GameState::FillHoles:
    downward = columns_with_holes;
    offset_value = 1 - (delay / delay_init);
    break;
  default:;
  }
  bool rotateLeft = state == GameState::RotateLeft;

  for (int x=0; x < width; ++x) {
    bool hole_found = false;
    for (int y=0; y < height; ++y) {
      switch (grid.get(x,y)) {
      case CellType::EMPTY  : hole_found = true; continue;
      case CellType::RED    : r.set_draw_color({248, 56, 0}); break;
      case CellType::BLUE   : r.set_draw_color({0, 120, 248}); break;
      case CellType::YELLOW : r.set_draw_color({248, 184, 0}); break;
      case CellType::GREEN  : r.set_draw_color({0, 168, 0}); break;
      case CellType::SIZE   : unreachable();
      }

      // Pixel coordinates of lower-left corner
      int px = x * cell_width  + margin;
      int py = y * cell_height + margin;
      // Pixel width and height of cell
      int pw = cell_width  - margin*2;
      int ph = cell_height - margin*2;

      int xy = y * grid.width + x;
      int off_x = 0;
      int off_y = 0;

      if (!offset.empty()) {
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

      if (!hole_found &&
          std::find(downward.begin(), downward.end(), x) != downward.end()) {
        off_y = offset_value * cell_height;
      }

      if (std::find(cells_in_match.begin(), cells_in_match.end(), xy) != cells_in_match.end()
          && frame % 3 == 0) {
        r.set_draw_color({255,255,255});
      }

      r.fill_rect(px + off_x, py + off_y, pw, ph);
    }
  }

  ++frame;
}

void Game::move_up() { selected_point.y = std::max(1, selected_point.y-1); }
void Game::move_down() { selected_point.y = std::min(height-1, selected_point.y+1); }
void Game::move_left() { selected_point.x = std::max(1, selected_point.x-1); }
void Game::move_right() { selected_point.x = std::min(width-1, selected_point.x+1); }

void Game::start_rotate(GameState next_state) {
  auto [x,y] = selected_point;
  // TODO: this can be precomputed
  cells_in_rotation = {
     y    * width + x,
     y    * width + x-1,
    (y-1) * width + x-1,
    (y-1) * width + x
  };
  set_state(next_state);
}

void Game::rotate_cells_right() {
  if (!cells_in_rotation.empty()) {
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

void Game::rotate_cells_left() {
  if (!cells_in_rotation.empty()) {
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

void Game::check_for_matches() {
  current_matches = check_all_matches();
  if (!current_matches.empty()) {
    for (auto& m: current_matches) {
      cells_in_match.insert(cells_in_match.end(),
                            m.cells.begin(), m.cells.end());
    }
    set_state(GameState::PreHighlightMatchCells);
  }
}

std::vector<Match> Game::check_all_matches() {
  std::vector<Match> matches;

  for (int y=0; y < height; ++y) {
    for (int x=0; x < width; ++x) {
      for (auto& p : patterns) {
        auto cells = match_pattern_at(p, x, y);
        if (!cells.empty()) {
          Match m = {p, cells};
          matches.push_back(m);
        }
      }
    }
  }

  return matches;
}

PatternMatchType match_pattern_char(char c, CellType t) {
  auto N = PatternMatchType::NO_MATCH;
  auto Y = PatternMatchType::MATCH;

  switch (c) {
  case '0': return t == CellType::RED    ? Y : N;
  case '1': return t == CellType::BLUE   ? Y : N;
  case '2': return t == CellType::YELLOW ? Y : N;
  case '3': return t == CellType::GREEN  ? Y : N;
  case '.': return PatternMatchType::PASS;
  default: return N;
  }
}

std::vector<int> Game::match_pattern_at(Pattern& pattern, int x, int y) {
  int h = pattern.size();
  int w = pattern[0].size();
  std::vector<int> match;

  // Bail if the pattern is too large to fit
  if (x + w > width || y + h > height) {
    return match;
  }

  for (int yy=0; yy < h; ++yy) {
    for (int xx=0; xx < w; ++xx) {
      auto c = grid.get(x + xx, y + yy);
      auto m = match_pattern_char(pattern[yy][xx], c);
      switch (m) {
      case PatternMatchType::NO_MATCH:
        return {};
      case PatternMatchType::MATCH:
        match.push_back((y + yy) * width + x + xx);
        break;
      case PatternMatchType::PASS: break;
      }
    }
  }

  return match;
}

void Game::remove_match_cells() {
  assert(!cells_in_match.empty());

  columns_with_holes.clear();

  for (auto c: cells_in_match) {
    grid.cells[c] = CellType::EMPTY;
    int col = c % width;
    if (std::find(columns_with_holes.begin(), columns_with_holes.end(), col) == columns_with_holes.end()) {
      columns_with_holes.push_back(col);
    }
  }

  cells_in_match.clear();

  // TODO: score matches depending on type

  current_matches.clear();
}

void Game::fill_holes() {
  for (auto c: columns_with_holes) {
    for (int y=0; y < height; ++y) {
      if (grid.get(c, y) == CellType::EMPTY) {
        grid.push_down(c, y, grid.next_random_cell());
        break;
      }
    }
  }

  // Now check for columns with holes still
  std::vector<int> new_cols;
  for (auto c: columns_with_holes) {
    for (int y=0; y < height; ++y) {
      if (grid.get(c, y) == CellType::EMPTY) {
        new_cols.push_back(c);
        break;
      }
    }
  }

  columns_with_holes = new_cols;
}

CellType Grid::push_down(int column, int row, CellType new_cell) {
  auto out = get(column, row);

  int y = row;
  for (; y > 0; --y) {
    put(get(column, y-1), column, y);
  }
  put(new_cell, column, y);

  return out;
}
