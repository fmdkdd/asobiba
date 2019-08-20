// TODO: Game launches, can move but nothing appears when shooting and only one
// enemy that doesn't move.
// TODO: XRA A,A does not set zero flag?
// TODO: does CPI set CY correctly?

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <SDL2/SDL.h>

#include "cpu.h"
#include "debug.h"
#include "jit.h"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t i32;

void die(const char *const msg) {
  perror(msg);
  exit(1);
}

void sdl_die(const char *const msg) {
  SDL_Log("%s: %s\n", msg, SDL_GetError());
  exit(1);
}

int main(const int argc, const char* const argv[]) {

  if (argc != 2) {
    fprintf(stderr, "Usage: spinv ROM");
    exit(1);
  }

  const char *path = argv[1];

#ifdef CPUDIAG
  int orig = 0x100;
#else
  int orig = 0;
#endif

  // Init CPU
  CPU cpu;
  memset(&cpu, 0, sizeof(CPU));

  // Map ROM
  FILE *const rom = fopen(path, "rb");
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
  i32 cycles = 0;

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

  SDL_Renderer *const renderer =
    SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
  if (!renderer)
    sdl_die("Could not create renderer");

  jit_init();

  cpu.ports[1] = 1 << 3;
  cpu.ports[2] = 0x81;

  while (true) {
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT: goto done;

      case SDL_KEYDOWN:
        switch (e.key.keysym.scancode) {
        case SDL_SCANCODE_C:      cpu.ports[1] |= 1 << 0; break; // COIN
        case SDL_SCANCODE_RSHIFT: cpu.ports[1] |= 1 << 1; break; // 2P start
        case SDL_SCANCODE_LSHIFT: cpu.ports[1] |= 1 << 2; break; // 1P start
        case SDL_SCANCODE_S:      cpu.ports[1] |= 1 << 4; break; // 1P shoot
        case SDL_SCANCODE_A:      cpu.ports[1] |= 1 << 5; break; // 1P left
        case SDL_SCANCODE_D:      cpu.ports[1] |= 1 << 6; break; // 1P right

        case SDL_SCANCODE_K:      cpu.ports[2] |= 1 << 4; break; // 2P shoot
        case SDL_SCANCODE_J:      cpu.ports[2] |= 1 << 5; break; // 2P left
        case SDL_SCANCODE_L:      cpu.ports[2] |= 1 << 6; break; // 2P right

        default: break;
        }
        break;

      case SDL_KEYUP:
        switch (e.key.keysym.scancode) {
        case SDL_SCANCODE_C:      cpu.ports[1] &= ~(1 << 0); break; // COIN
        case SDL_SCANCODE_RSHIFT: cpu.ports[1] &= ~(1 << 1); break; // 2P start
        case SDL_SCANCODE_RETURN: cpu.ports[1] &= ~(1 << 2); break; // 1P start
        case SDL_SCANCODE_S:      cpu.ports[1] &= ~(1 << 4); break; // 1P shoot
        case SDL_SCANCODE_A:      cpu.ports[1] &= ~(1 << 5); break; // 1P left
        case SDL_SCANCODE_D:      cpu.ports[1] &= ~(1 << 6); break; // 1P right

        case SDL_SCANCODE_K:      cpu.ports[2] &= ~(1 << 4); break; // 2P shoot
        case SDL_SCANCODE_J:      cpu.ports[2] &= ~(1 << 5); break; // 2P left
        case SDL_SCANCODE_L:      cpu.ports[2] &= ~(1 << 6); break; // 2P right

        case SDL_SCANCODE_ESCAPE: goto done;

        default: break;
        }
        break;

      }
    }

    // Emulate for 1/60 second
    BEGIN_TIME(emulate);
    cpu_interrupt(&cpu);
    // TODO: should run for the actual elapsed time since the last frame
    cycles += 33333;
    while (cycles > 0) {
      cycles -= jit_run(&cpu);
    }
    END_TIME(emulate);

    // Draw content of video RAM
    SDL_SetRenderDrawColor(renderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, SDL_ALPHA_OPAQUE);

    for (u32 y=0; y < 256; ++y)
      for (u32 x=0; x < 32; ++x)
        for (u8 b=0; b < 8; ++b)
          if ((cpu.ram[0x2400 + y*32 + x] >> b) & 1)
            // Display is rotated 90 degree counter-clockwise
            SDL_RenderDrawPoint(renderer, y, 256-(8*x + b));

    SDL_RenderPresent(renderer);
  }

 done:

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  jit_dump_hot_routines(&cpu);

  return EXIT_SUCCESS;
}
