#pragma once

#include <random>
#include <vector>

#include "sdl.h"

static const double pi = std::acos(-1);

enum Key {
  UP, DOWN, LEFT, RIGHT, Z, X, F, G,
  SIZE
};

struct Point {
  float x;
  float y;

  Point operator+(Point b) { return {x+b.x, y+b.y}; }
  void operator+=(Point b) { x+=b.x; y+=b.y; }
  Point operator-(Point b) { return {x-b.x, y-b.y}; }
  Point operator*(float s) { return {x*s, y*s}; }
  Point operator/(float s) { return {x/s, y/s}; }
  void operator/=(float s) { x/=s; y/=s; }
  float dot(Point b) { return x*b.x + y*b.y; }
  float mag() { return std::sqrt(x*x + y*y); }
};

struct Ball {
  Point pos;
  Point vel;
  bool alive;

  bool hit;
  static constexpr int size = 10;

  void reset() {
    pos = {150,10};
    vel = {50.0f*((float)std::rand()/RAND_MAX),250};
    alive = true;
  }
};

struct Player {
  Point pos {150, 300};
  float rot = 0;
  int size = 30;
  int faces = 4;
  std::vector<Point> shape;
  bool hit;
};

struct Game {
  static constexpr int width = 350;
  static constexpr int height = 500;

  SDL_Scancode upkey;

  bool keys[Key::SIZE] = {false};

  Player player;
  std::vector<Ball> balls;

  Game() {
    for (int i=0; i < 20; ++i) {
      Ball b;
      b.reset();
      balls.push_back(b);
    }
  }

  void sdl_event(SDL_Event& e);
  void update(double dt);
  void render(SDLRenderer& r);
};
