#include "scene.h"

#include <chrono>

void SceneDemo::enter(Gfx& gfx) {
  gfx.loadImage("data/lemmings-spritesheet.png", &lemmingsSpritesheet);
}

void SceneDemo::leave() {
  free(lemmings[0].spriteRects);
  lemmings[0].spriteRects = NULL;

  lemmingsSpritesheet.quit();
}

void SceneDemo::reset() {
  // TODO: move to init

  // Lemmings
  AnimatedSprite walkingLemming;
  walkingLemming.spritesheet = &lemmingsSpritesheet;
  walkingLemming.spriteRectsCount = 7;
  walkingLemming.spriteRects =
      (Recti *)malloc(sizeof(Recti) * walkingLemming.spriteRectsCount);
  walkingLemming.animationSpeed = 110;
  walkingLemming.animationStepCounter = 0;
  {
    Recti src{0, 0, 20, 20};
    for (usize i = 0; i < walkingLemming.spriteRectsCount; ++i) {
      walkingLemming.spriteRects[i] = src;
      src.x += 20;
    }
  }

  for (usize i = 0; i < K_ARRAY_SIZE(lemmings); ++i) {
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
  pong.ballSpeed = 2000;
  pong.ballCounter = 0;
  pong.ballSpeedIncreaseOnHit = 100;
  pong.ballSpeedMax = 3000;

  pong.player1Speed = 5000;
  pong.player1SpeedCounter = 0;
  pong.player1Velocity = 1;
  pong.player2Speed = 5000;
  pong.player2SpeedCounter = 0;
  pong.player2Velocity = 1;
}

void SceneDemo::update(const Controls &controls) {
  if (controls.wasGameKeyPressed(KEY_PAUSE)) {
    m_pGame->isPaused = !m_pGame->isPaused;
  }

  if (m_pGame->isPaused)
    return;

  if (m_pGame->m_useLemmings) {
    for (usize i = 0; i < K_ARRAY_SIZE(lemmings); ++i) {
      lemmings[i].step();
    }
  }

  // Pong
  pong.update(controls);

  m_pGame->time += 1;
}

void Pong::update(const Controls &controls) {
  player1SpeedCounter += player1Speed;
  while (player1SpeedCounter >= 1000) {
    player1SpeedCounter -= 1000;

    if (controls.isKeyDown(KEY_PLAYER1_UP)) {
      player1Bat.y += player1Velocity;
    } else if (controls.isKeyDown(KEY_PLAYER1_DOWN)) {
      player1Bat.y -= player1Velocity;
    }

    if (player1Bat.y < arena.y) {
      player1Bat.y = arena.y;
    }
    if ((player1Bat.y + player1Bat.h) > (arena.y + arena.h)) {
      player1Bat.y = arena.y + arena.h - player1Bat.h;
    }
  }

  player2SpeedCounter += player2Speed;
  while (player2SpeedCounter >= 1000) {
    player2SpeedCounter -= 1000;

    if (controls.isKeyDown(KEY_PLAYER2_UP)) {
      player2Bat.y += player2Velocity;
    } else if (controls.isKeyDown(KEY_PLAYER2_DOWN)) {
      player2Bat.y -= player2Velocity;
    }

    if (player2Bat.y < arena.y) {
      player2Bat.y = arena.y;
    }
    if ((player2Bat.y + player2Bat.h) > (arena.y + arena.h)) {
      player2Bat.y = arena.y + arena.h - player2Bat.h;
    }
  }

  ballCounter += ballSpeed;
  while (ballCounter >= 1000) {
    ballCounter -= 1000;

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

    if (ball.intersectsWith(player1Bat)) {
      ball.x = player1Bat.x + player1Bat.w;
      ballVelocity.x = -ballVelocity.x;
      ballSpeed += ballSpeedIncreaseOnHit;
      if (ballSpeed >= ballSpeedMax)
        ballSpeed = ballSpeedMax;
    }
    if (ball.intersectsWith(player2Bat)) {
      ball.x = player2Bat.x - ball.w;
      ballVelocity.x = -ballVelocity.x;
      ballSpeed += ballSpeedIncreaseOnHit;
      if (ballSpeed >= ballSpeedMax)
        ballSpeed = ballSpeedMax;
    }
  }
}

void SceneDemo::render(Gfx &gfx) {
  const Controls &controls = *gfx.controls;

  static std::chrono::time_point<std::chrono::high_resolution_clock> last_frame;
  auto now = std::chrono::high_resolution_clock::now();
  auto dt = now - last_frame;
  last_frame = now;
  double dt_ms = (double)std::chrono::nanoseconds(dt).count() / 1000000;

  clearScreen(ColorRGBA{30, 30, 30, 255});

  setDrawColor(ColorRGBA{255, 255, 255, 255});

  gfx.text("Hello!", 16, 420);

  gfx.boxedText("I'm a text box", 16, 400);

  char buf[256];
  snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
  gfx.text(buf, 160, 400);

  snprintf(buf, sizeof(buf), "screen mouse: x: %d y: %d",
           controls.lastScreenMousePosition.x,
           controls.lastScreenMousePosition.y);
  gfx.text(buf, 140, 380);

  snprintf(buf, sizeof(buf), "logical mouse: x: %d y: %d",
           controls.lastLogicalMousePosition.x,
           controls.lastLogicalMousePosition.y);
  gfx.text(buf, 130, 360);

  drawRect(controls.lastLogicalMousePosition.x,
           controls.lastLogicalMousePosition.y, 10, 10);

  if (gfx.button("And I'm a button", 16, 328)) {
    gfx.text("Stop clicking!", 16, 310);
  }

  if (m_pGame->m_useLemmings) {
    for (int i = 0; i < 10; ++i) {
      lemmings[i].drawAt(10 + 20 * i, 250);
    }
  }

  // Pong
  drawRect(pong.arena.x, pong.arena.y, pong.arena.w, pong.arena.h);
  fillRect(pong.player1Bat.x, pong.player1Bat.y, pong.player1Bat.w,
           pong.player1Bat.h);
  fillRect(pong.player2Bat.x, pong.player2Bat.y, pong.player2Bat.w,
           pong.player2Bat.h);
  fillRect(pong.ball.x, pong.ball.y, pong.ball.w, pong.ball.h);

  const float d = 0.2f;

  Vec2f curve[1000];
  for (size_t i = 0; i < K_ARRAY_SIZE(curve); ++i) {
    Vec2f &v = curve[i];
    v.x = i;
    v.y = 520 + 20 * (sin(d * (4 * m_pGame->time + i)) + 2 * sin(0.25 * (4 * m_pGame->time + i)));
  }
  drawLine(curve, K_ARRAY_SIZE(curve), 2.0f);

  if (m_pGame->isPaused) {
    setDrawColor(ColorRGBA{0, 0, 0, 120});
    fillRect(0, 0, 800, 600);
    setDrawColor(ColorRGBA{255, 255, 255, 255});
    gfx.text("Paused", 400, 300);
  }
}
