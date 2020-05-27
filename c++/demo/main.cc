#include <chrono>

#include "sdl.h"

int main() {
  const u32 logical_width  = 800;
  const u32 logical_height = 450;
  const u32 scale = 1;

  const u32 window_width = logical_width * scale;
  const u32 window_height = logical_height * scale;

  SDL sdl;
  SDLWindow window("SDL demo", window_width, window_height);
  SDLRenderer renderer(window, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
  renderer.set_scale(scale, scale);
  SDL_RenderSetIntegerScale(renderer.renderer, SDL_TRUE);
  SDL_RenderSetLogicalSize(renderer.renderer, logical_width, logical_height);

  auto last_frame = std::chrono::high_resolution_clock::now();

  while (true) {
    renderer.commitInputState();

    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;
      case SDL_KEYUP:
        if (e.key.keysym.scancode == SDL_SCANCODE_ESCAPE)
          goto done;
        break;

      case SDL_MOUSEMOTION:
        renderer.updateMousePosition(e.motion.x, e.motion.y);
        break;

      case SDL_MOUSEBUTTONDOWN:
      case SDL_MOUSEBUTTONUP:
        renderer.updateMouseButton(e.button.button, e.button.state);
        break;
      }
    }

    auto now = std::chrono::high_resolution_clock::now();
    auto dt = now - last_frame;
    last_frame = now;
    double dt_ms = (double) std::chrono::nanoseconds(dt).count() / 1'000'000;

    renderer.set_draw_color({30,30,30});
    renderer.clear();

    renderer.set_draw_color({255,255,255});
    renderer.draw_rect(0,0, logical_width, logical_height);

    renderer.text("Hello!", 16, 16);

    renderer.boxed_text("I'm a text box", 16, 48);

    char buf[256];
    snprintf(buf, sizeof(buf), "Frame time: %fs", dt_ms);
    renderer.text(buf, 160, 20);

    int mouse_x;
    int mouse_y;
    SDL_GetMouseState(&mouse_x, &mouse_y);
    snprintf(buf, sizeof(buf), "mouse: x: %d y: %d", mouse_x, mouse_y);
    renderer.text(buf, 160, 40);

    snprintf(buf, sizeof(buf), "logical mouse: x: %d y: %d", renderer.lastMousePosition.x, renderer.lastMousePosition.y);
    renderer.text(buf, 120, 80);

    renderer.draw_rect(renderer.lastMousePosition.x, renderer.lastMousePosition.y, 10, 10);

    if (renderer.button("And I'm a button", 16, 128)) {
      renderer.text("Stop clicking!", 16, 144);
    }

    const bool isFullscreen = SDL_GetWindowFlags(window.window) & SDL_WINDOW_FULLSCREEN_DESKTOP;

    if (isFullscreen) {
      if (renderer.button("Windowed", 16, 300)) {
        SDL_SetWindowFullscreen(window.window, 0);
      }
    }
    else {
      if (renderer.button("Fullscreen", 16, 300)) {
        SDL_SetWindowFullscreen(window.window, SDL_WINDOW_FULLSCREEN_DESKTOP);
      }
    }

    // TODO:
    // - Use textures (since drawing with SDL is limited, blitting PNGs
    //   should be the default besides text)
    // - Live reloading with game compiled as dynamically loaded lib
    // - Drag&drop
    // - Dialog boxes
    // - Menu with 50% alpha overlay
    // - Do not structure too much in order to leave
    //   maximum flexiblity for prototyping (no framework)
    // - UI animations?
    // - Screen shake
    // - ImGui integration

    renderer.present();
  }

 done:
  return EXIT_SUCCESS;
}
