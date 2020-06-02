#ifndef GAME_API_H
#define GAME_API_H

class SDLRenderer;
struct Game;

struct GameAPI {
  Game* (*Init)(SDLRenderer& renderer);
  void  (*Quit)(Game* game);
  void  (*Update)(Game* game, SDLRenderer& renderer);
};

extern const GameAPI GAME_API;

#endif
