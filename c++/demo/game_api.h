#ifndef GAME_API_H
#define GAME_API_H

struct SDL_Renderer;
struct Game;

struct GameAPI {
  Game* (*Init)(SDL_Renderer* renderer);
  void  (*Quit)(Game* game);
  void  (*Update)(Game* game, SDL_Renderer* renderer);
};

extern const GameAPI GAME_API;

#endif
