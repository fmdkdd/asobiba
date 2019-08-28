#include <algorithm>
#include <cmath>

#include "game.h"

void Game::sdl_event(SDL_Event& e) {
  bool down = true;

  switch (e.type) {
  case SDL_KEYUP:
    down = false;
    // fall-through
  case SDL_KEYDOWN:
    if (!e.key.repeat) {
      switch (e.key.keysym.scancode) {
      case SDL_SCANCODE_I:
      case SDL_SCANCODE_UP    : keys[Key::UP]    = down; break;
      case SDL_SCANCODE_K:
      case SDL_SCANCODE_DOWN  : keys[Key::DOWN]  = down; break;
      case SDL_SCANCODE_J:
      case SDL_SCANCODE_LEFT  : keys[Key::LEFT]  = down; break;
      case SDL_SCANCODE_L:
      case SDL_SCANCODE_RIGHT : keys[Key::RIGHT] = down; break;
      case SDL_SCANCODE_Z     : keys[Key::Z]     = down; break;
      case SDL_SCANCODE_X     : keys[Key::X]     = down; break;
      case SDL_SCANCODE_F     : keys[Key::F]     = down; break;
      case SDL_SCANCODE_G     : keys[Key::G]     = down; break;

      default:;
      }
    }
    break;
  }
}

bool intersects(Point pos1, int size1, Point pos2, int size2) {
  Point corners2[] = {{pos2.x, pos2.y},
                      {pos2.x + size2, pos2.y},
                      {pos2.x, pos2.y + size2},
                      {pos2.x + size2, pos2.y + size2}};

  for (auto c : corners2) {
    if (c.x >= pos1.x && c.x <= pos1.x + size1
        && c.y >= pos1.y && c.y <= pos1.y + size1)
      return true;
  }

  Point corners1[] = {{pos1.x, pos1.y},
                      {pos1.x + size1, pos1.y},
                      {pos1.x, pos1.y + size1},
                      {pos1.x + size1, pos1.y + size1}};

  for (auto c : corners1) {
    if (c.x >= pos2.x && c.x <= pos2.x + size2
        && c.y >= pos2.y && c.y <= pos2.y + size2)
      return true;
  }

  return false;
}

bool intersects_line(Point a, Point b, Point box, int size) {
  // Walk AB in segments smaller than size to find intersections with box.
  Point d = b - a;
  int steps = (d.mag() / ((float)size/2.0))+1;
  d /= steps;
  while (steps-- > 0) {
    if (a.x >= box.x && a.x <= box.x + size
        && a.y >= box.y && a.y <= box.y + size) {
      return true;
    }
    a += d;
  }

  return false;
}


void Game::update(double dt) {
  float v = 200 * dt;
  float th = 4 * dt;

  if (keys[Key::UP])    player.pos.y -= v;
  if (keys[Key::DOWN])  player.pos.y += v;
  if (keys[Key::LEFT])  player.pos.x -= v;
  if (keys[Key::RIGHT]) player.pos.x += v;
  if (keys[Key::Z]) player.rot -= th;
  if (keys[Key::X]) player.rot += th;

  if (keys[Key::F]) {
    player.faces++;
    keys[Key::F] = false;
  }
  if (keys[Key::G]) {
    player.faces--;
    keys[Key::G] = false;
  }

  // Update player shape
  auto [x,y] = player.pos;
  float a = player.rot;
  float da = 2*pi/player.faces;
  player.shape.clear();
  for (int i=0; i < player.faces; ++i) {
    player.shape.push_back({std::cos(a) * player.size, std::sin(a) * player.size});
    a += da;
  }

  player.hit = false;

  // Update balls
  {
    for (auto& b : balls) {
      b.pos.x += b.vel.x * dt;
      b.pos.y += b.vel.y * dt;
      if (b.pos.x < 0 || b.pos.x >= width
          || b.pos.y < 0 || b.pos.y >= height) {
        b.alive = false;
      }

      b.hit = false;

      // Detect intersection of the ball with each line of the player shape, and
      // bounce off the incident angle.
      auto i = 0ul;
      for (; i < player.shape.size(); ++i) {
        Point pa = {player.pos.x + player.shape[i].x,
                    player.pos.y + player.shape[i].y};

        Point pb;
        if (i < player.shape.size()-1) {
          pb = {player.pos.x + player.shape[i+1].x,
                player.pos.y + player.shape[i+1].y};
        } else {
          pb = {player.pos.x + player.shape[0].x,
                player.pos.y + player.shape[0].y};
        }

        if (intersects_line(pa, pb, b.pos, b.size)) {
          player.hit = b.hit = true;

          // Bounce
          Point n = pb - pa;
          n = {-n.y, n.x};
          n /= n.mag();
          b.vel = n * (-2) * b.vel.dot(n) + b.vel;
        }
      }
    }
  }

  for (auto& b : balls) {
    if (!b.alive) {
      b.reset();
    }
  }
}

void Game::render(SDLRenderer& r) {
  r.set_draw_color({0,0,0});
  r.clear();

  // Draw player
  auto [x,y] = player.pos;
  float a = player.rot;
  float da = 2*pi/player.faces;
  std::vector<Point> p;
  for (int i=0; i < player.faces; ++i) {
    p.push_back({std::cos(a) * player.size, std::sin(a) * player.size});
    a += da;
  }

  if (player.hit)
    r.set_draw_color({255,255,0});
  else
    r.set_draw_color({255,255,255});

  auto i = 0ul;
  for (; i < p.size()-1; ++i) {
    auto p0 = p[i];
    auto p1 = p[i+1];
    r.draw_line(x + p0.x, y + p0.y, x + p1.x, y + p1.y);
  }
  r.draw_line(x + p[i].x, y + p[i].y, x + p[0].x, y + p[0].y);

  // Draw balls
  r.set_draw_color({255,0,0});
  for (auto& b : balls) {
    if (b.alive) {
      if (b.hit)
        r.fill_rect(b.pos.x, b.pos.y, b.size, b.size);
      else
        r.draw_rect(b.pos.x, b.pos.y, b.size, b.size);
    }
  }
}
