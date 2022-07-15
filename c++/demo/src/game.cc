#include "game.h"

#include "app.h"
#include "game_api.h"
#include "utils.h"

void AnimatedSprite::drawAt(SDL_Renderer *renderer, u32 x, u32 y) {
  SDL_Rect dst;
  dst.x = x;
  dst.y = y;
  dst.w = 40;
  dst.h = 40;
  ASSERT(animationStep < spriteRectsCount);
  SDL_RenderCopy(renderer, spritesheet, &spriteRects[animationStep], &dst);
}

void AnimatedSprite::step(u32 ticks) {
  animationStepCounter += ticks;
  while (animationStepCounter >= animationSpeed) {
    animationStepCounter -= animationSpeed;
    animationStep++;
    if (animationStep >= spriteRectsCount)
      animationStep = 0;
  }
}

void Game::init(App &app) {
  lemmingsSpritesheet = app.loadImage("data/lemmings-spritesheet.png");

  reset();
}

void Game::reset() {
  // Lemmings
  AnimatedSprite walkingLemming;
  walkingLemming.spritesheet = lemmingsSpritesheet;
  walkingLemming.spriteRectsCount = 7;
  walkingLemming.spriteRects =
      (SDL_Rect *)malloc(sizeof(SDL_Rect) * walkingLemming.spriteRectsCount);
  walkingLemming.animationSpeed = 100;
  walkingLemming.animationStepCounter = 0;
  {
    SDL_Rect src;
    src.x = 0;
    src.y = 0;
    src.w = 20;
    src.h = 20;
    for (usize i = 0; i < walkingLemming.spriteRectsCount; ++i) {
      walkingLemming.spriteRects[i] = src;
      src.x += 20;
    }
  }

  for (usize i = 0; i < ARRAY_SIZE(lemmings); ++i) {
    lemmings[i] = walkingLemming;
    lemmings[i].animationStep = rand() % 7;
    lemmings[i].animationSpeed = walkingLemming.animationSpeed;
  }

  // Pong
  pong.arena = {380, 20, 400, 400};
  pong.player1Bat = {390, 30, 20, 80};
  pong.player2Bat = {750, 330, 20, 80};
  pong.ball = {pong.arena.x + (pong.arena.w / 2),
               pong.arena.y + (pong.arena.h / 2), 10, 10};

  pong.ballVelocity = {1, 1};
  pong.ballSpeed = 10;
  pong.ballCounter = 0;

  pong.player1Speed = 3;
  pong.player1SpeedCounter = 0;
  pong.player1Velocity = 1;
  pong.player2Speed = 3;
  pong.player2SpeedCounter = 0;
  pong.player2Velocity = 1;
}

void Game::quit() {
  free(lemmings[0].spriteRects);
  lemmings[0].spriteRects = NULL;

  SDL_DestroyTexture(lemmingsSpritesheet);
}

bool Rect::collideWith(Rect &other) const {
  return x <= (other.x + other.w) && (x + w) >= other.x &&
         y <= (other.y + other.h) && (y + h) >= other.y;
}

void Pong::update(const App &app, u32 ticks) {
  player1SpeedCounter += ticks;
  while (player1SpeedCounter >= player1Speed) {
    player1SpeedCounter -= player1Speed;

    if (app.isKeyHeld(KEY_PLAYER1_UP)) {
      player1Bat.y -= player1Velocity;
    } else if (app.isKeyHeld(KEY_PLAYER1_DOWN)) {
      player1Bat.y += player1Velocity;
    }

    if (player1Bat.y < arena.y) {
      player1Bat.y = arena.y;
    }
    if ((player1Bat.y + player1Bat.h) > (arena.y + arena.h)) {
      player1Bat.y = arena.y + arena.h - player1Bat.h;
    }
  }

  player2SpeedCounter += ticks;
  while (player2SpeedCounter >= player2Speed) {
    player2SpeedCounter -= player2Speed;

    if (app.isKeyHeld(KEY_PLAYER2_UP)) {
      player2Bat.y -= player2Velocity;
    } else if (app.isKeyHeld(KEY_PLAYER2_DOWN)) {
      player2Bat.y += player2Velocity;
    }

    if (player2Bat.y < arena.y) {
      player2Bat.y = arena.y;
    }
    if ((player2Bat.y + player2Bat.h) > (arena.y + arena.h)) {
      player2Bat.y = arena.y + arena.h - player2Bat.h;
    }
  }

  ballCounter += ticks;
  while (ballCounter >= ballSpeed) {
    ballCounter -= ballSpeed;

    ball.x += ballVelocity.x;
    ball.y += ballVelocity.y;

    if ((ball.x + ball.w) > (arena.x + arena.w)) {
      ball.x = arena.x + arena.w - ball.w;
      ballVelocity.x = -ballVelocity.x;
    }
    if (ball.x < arena.x) {
      ball.x = arena.x;
      ballVelocity.x = -ballVelocity.x;
    }
    if ((ball.y + ball.h) > (arena.y + arena.h)) {
      ball.y = arena.y + arena.h - ball.h;
      ballVelocity.y = -ballVelocity.y;
    }
    if (ball.y < arena.y) {
      ball.y = arena.y;
      ballVelocity.y = -ballVelocity.y;
    }

    if (ball.collideWith(player1Bat)) {
      ball.x = player1Bat.x + player1Bat.w;
      ballVelocity.x = -ballVelocity.x;
    }
    if  (ball.collideWith(player2Bat)) {
      ball.x = player2Bat.x - ball.w;
      ballVelocity.x = -ballVelocity.x;
    }
  }
}

void Game::update(const App &app, u32 ticks) {
  for (usize i = 0; i < ARRAY_SIZE(lemmings); ++i) {
    lemmings[i].step(ticks);
  }

  // Pong
  pong.update(app, ticks);
}

void Game::render(App &app) {
  static std::chrono::time_point<std::chrono::high_resolution_clock> last_frame;
  auto now = std::chrono::high_resolution_clock::now();
  auto dt = now - last_frame;
  last_frame = now;
  double dt_ms = (double)std::chrono::nanoseconds(dt).count() / 1000000;

  app.setRenderDrawColor(ColorRGBA{30, 30, 30, 255});
  app.renderClear();

  app.setRenderDrawColor(ColorRGBA{255, 255, 255, 255});

  app.text("Hello!", 16, 16);

  app.boxedText("I'm a text box", 16, 48);

  char buf[256];
  snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
  app.text(buf, 160, 20);

  int mouse_x;
  int mouse_y;
  SDL_GetMouseState(&mouse_x, &mouse_y);
  snprintf(buf, sizeof(buf), "mouse: x: %d y: %d", mouse_x, mouse_y);
  app.text(buf, 160, 40);

  snprintf(buf, sizeof(buf), "logical mouse: x: %d y: %d",
           app.lastMousePosition.x, app.lastMousePosition.y);
  app.text(buf, 120, 80);

  app.drawRect(app.lastMousePosition.x, app.lastMousePosition.y, 10, 10);

  if (app.button("And I'm a super button", 16, 128)) {
    app.text("Stop clicking!", 16, 144);
  }

  for (int i = 0; i < 10; ++i) {
    lemmings[i].drawAt(app.renderer, 10 + 20 * i, 200);
  }

  // Pong
  app.drawRect(pong.arena.x, pong.arena.y, pong.arena.w, pong.arena.h);
  app.fillRect(pong.player1Bat.x, pong.player1Bat.y, pong.player1Bat.w,
               pong.player1Bat.h);
  app.fillRect(pong.player2Bat.x, pong.player2Bat.y, pong.player2Bat.w,
               pong.player2Bat.h);
  app.fillRect(pong.ball.x, pong.ball.y, pong.ball.w, pong.ball.h);
}

Game *gameInit(App &app) {
  Game *game = (Game *)malloc(sizeof(Game));
  ENSURE(game != NULL);
  game->init(app);
  return game;
}

void gameQuit(Game *game) {
  game->quit();
  free(game);
}

void gameReset(Game *game) { game->reset(); }

void gameUpdate(Game *game, const App &app, u32 ticks) {
  game->update(app, ticks);
}
void gameRender(Game *game, App &app) { game->render(app); }

const GameAPI GAME_API = {
    gameInit, gameQuit, gameReset, gameUpdate, gameRender,
};
