// TODO: XRA A,A does not set zero flag?
// TODO: does CPI set CY correctly?

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <SDL2/SDL.h>

#include "common.h"
#include "cpu.h"
#include "debug.h"
#include "jit.h"

extern char *optarg;
extern int optind;

enum input
  {
   INPUT_COIN     = 1 << 0,
   INPUT_2P_START = 1 << 1,
   INPUT_1P_START = 1 << 2,
   INPUT_1P_SHOOT = 1 << 4,
   INPUT_1P_LEFT  = 1 << 5,
   INPUT_1P_RIGHT = 1 << 6,
   INPUT_2P_SHOOT = 1 << 4,
   INPUT_2P_LEFT  = 1 << 5,
   INPUT_2P_RIGHT = 1 << 6,
  };

void sdl_die(const char *const msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

void print_usage_and_die() {
  fprintf(stderr, "Usage: spinv OPTIONS ROM\n\n"
          "Options:\n"
          "-v       sync to vblank\n"
          "-c file  record inputs\n"
          "-r file  replay inputs\n");
  exit(1);
}

int main(const int argc, const char* argv[]) {

  int opt;
  bool vsync = false;
  bool replay = false;
  bool record = false;
  const char* replay_path;

  while ((opt = getopt(argc, argv, "vr:c:")) != -1) {
    switch (opt) {
    case 'v':
      vsync = true;
      break;
    case 'r':
      replay_path = optarg;
      replay = true;
      break;
    case 'c':
      replay_path = optarg;
      record = true;
      break;
    default:
      print_usage_and_die();
    }
  }

  if (optind >= argc) {
    print_usage_and_die();
  }

  const char *rom_path = argv[optind];

#ifdef CPUDIAG
  int orig = 0x100;
#else
  int orig = 0;
#endif

  // Init CPU
  CPU cpu;
  cpu_init(&cpu);

  // Map ROM
  FILE *const rom = fopen(rom_path, "rb");
  if (!rom) die("Cannot open file ");
  fread(cpu.ram + orig, sizeof(u8), 0x2000, rom);
  fclose(rom);

#ifdef CPUDIAG
  // Tweak the code so that instead of printing messages,
  // we exit with failure or success
  cpu.ram[0x0689] = 0xfd; // CPUER;
  cpu.ram[0x069b] = 0xed; // CPUOK;
#endif

  cpu.pc = orig;

  // Init SDL
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    sdl_die("Unable to initialize SDL");

  SDL_Window *const window =
    SDL_CreateWindow("Spinv",
                     SDL_WINDOWPOS_UNDEFINED,
                     SDL_WINDOWPOS_UNDEFINED,
                     224, 256,
                     0);
  if (!window)
    sdl_die("Could not create window");

  SDL_RendererFlags renderer_flags = SDL_RENDERER_ACCELERATED;
  if (vsync)
    renderer_flags |= SDL_RENDERER_PRESENTVSYNC;

  SDL_Renderer *const renderer =
    SDL_CreateRenderer(window, -1, renderer_flags);
  if (!renderer)
    sdl_die("Could not create renderer");
  SDL_RendererInfo info;
  SDL_GetRendererInfo(renderer, &info);
  printf("SDL Renderer: %s (accelerated: %s, vsync: %s)\n",
         info.name,
         info.flags & SDL_RENDERER_ACCELERATED ? "yes" : "no",
         info.flags & SDL_RENDERER_PRESENTVSYNC ? "yes" : "no");

  jit_init();

  cpu.ports[1] = 1 << 3;
  cpu.ports[2] = 0x03; // 6 lives, I'm a wuss

#ifndef BENCH
  u64 last_render_time = cpu_time_as_nanoseconds();
#endif

  while (true) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;

      case SDL_KEYDOWN:
        switch (e.key.keysym.scancode) {
        case SDL_SCANCODE_C:      cpu.ports[1] |= INPUT_COIN;     break;
        case SDL_SCANCODE_RSHIFT: cpu.ports[1] |= INPUT_2P_START; break;
        case SDL_SCANCODE_LSHIFT: cpu.ports[1] |= INPUT_1P_START; break;
        case SDL_SCANCODE_S:      cpu.ports[1] |= INPUT_1P_SHOOT; break;
        case SDL_SCANCODE_A:      cpu.ports[1] |= INPUT_1P_LEFT;  break;
        case SDL_SCANCODE_D:      cpu.ports[1] |= INPUT_1P_RIGHT; break;

        case SDL_SCANCODE_K:      cpu.ports[2] |= INPUT_2P_SHOOT; break;
        case SDL_SCANCODE_J:      cpu.ports[2] |= INPUT_2P_LEFT;  break;
        case SDL_SCANCODE_L:      cpu.ports[2] |= INPUT_2P_RIGHT; break;

        default: break;
        }
        break;

      case SDL_KEYUP:
        switch (e.key.keysym.scancode) {
        case SDL_SCANCODE_C:      cpu.ports[1] &= ~INPUT_COIN;     break;
        case SDL_SCANCODE_RSHIFT: cpu.ports[1] &= ~INPUT_2P_START; break;
        case SDL_SCANCODE_RETURN: cpu.ports[1] &= ~INPUT_1P_START; break;
        case SDL_SCANCODE_S:      cpu.ports[1] &= ~INPUT_1P_SHOOT; break;
        case SDL_SCANCODE_A:      cpu.ports[1] &= ~INPUT_1P_LEFT;  break;
        case SDL_SCANCODE_D:      cpu.ports[1] &= ~INPUT_1P_RIGHT; break;

        case SDL_SCANCODE_K:      cpu.ports[2] &= ~INPUT_2P_SHOOT; break;
        case SDL_SCANCODE_J:      cpu.ports[2] &= ~INPUT_2P_LEFT;  break;
        case SDL_SCANCODE_L:      cpu.ports[2] &= ~INPUT_2P_RIGHT; break;

        case SDL_SCANCODE_ESCAPE: goto done;

        default: break;
        }
        break;
      }
    }

    // Emulate for 1/60 second at 2MHz: 33333 cycles per frame
    // 1 frame = 256 scanlines
    // mid-vblank interrupt happens at  96 lines: cycle 12500
    // vblank     interrupt happens at 224 lines: cycle 29167
    // remaining cycles: 4166

    BEGIN_TIME(emulate);
    // TODO: should run for the actual elapsed time since the last frame
    cpu_run_for(&cpu, 12500);
    cpu_interrupt(&cpu, 1); // mid-vblank
    cpu_run_for(&cpu, 16667);
    cpu_interrupt(&cpu, 2); // vblank
    cpu_run_for(&cpu, 4166);
    END_TIME(emulate);

    // Draw content of video RAM
    BEGIN_TIME(draw);
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);

    for (u32 y=0; y < 256; ++y) {
      for (u32 x=0; x < 32; ++x) {
        // Fake color
        if (x < 8)
          SDL_SetRenderDrawColor(renderer, 0, 255, 0, SDL_ALPHA_OPAQUE);
        else if (x > 25)
          SDL_SetRenderDrawColor(renderer, 255, 128, 0, SDL_ALPHA_OPAQUE);
        else
          SDL_SetRenderDrawColor(renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);

        for (u8 b=0; b < 8; ++b) {
          if ((cpu.ram[0x2400 + y*32 + x] >> b) & 1)
            // Display is rotated 90 degree counter-clockwise
            SDL_RenderDrawPoint(renderer, y, 256-(8*x + b));
        }
      }
    }
    END_TIME(draw);

    SDL_RenderPresent(renderer);

#ifndef BENCH
    {
      u64 now = cpu_time_as_nanoseconds();
      u64 delta = now - last_render_time;
      printf("%22s: %10luns\n", "frame time", delta);
      last_render_time = now;
    }
#endif
  }

 done:

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

#ifndef BENCH
  jit_dump_hot_routines(&cpu);
#endif

  return EXIT_SUCCESS;
}
