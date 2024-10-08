#ifndef GAME_API_H
#define GAME_API_H

struct Controls;
struct Gfx;
struct Game;

struct GameAPI {
  Game* (*init)(Gfx& gfx);
  void  (*quit)(Game* game);
  void  (*reset)(Game* game);
  void  (*update)(Game* game, const Controls& controls, Gfx& gfx);
  void  (*render)(Game* game, Gfx& gfx);
  void  (*renderImGui)(Game* game);
};

struct GameLib {
  void *handle;
  Game *state;
  GameAPI api;
};

extern const GameAPI GAME_API;

#endif
