#ifndef GAME_API_H
#define GAME_API_H

struct App;
struct Game;

struct GameAPI {
  Game* (*init)(App& app);
  void  (*quit)(Game* game);
  void  (*reset)(Game* game);
  void  (*update)(Game* game, const App& app, u32 ticks);
  void  (*render)(Game* game, App& app);
};

struct GameLib {
  void *handle;
  Game *state;
  GameAPI api;
};

extern const GameAPI GAME_API;

#endif
