#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <SDL2/SDL.h>

#include "cpu.h"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

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
  // Tweak the code so we directly fail when jumping to CPUER
  cpu.ram[0x0689] = 0xfd; // CPUER;
#endif

  cpu.pc = orig;
  u32 cycles = 0;

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

  while (true) {
    SDL_Event e;
    if (SDL_PollEvent(&e)) {
      if (e.type == SDL_QUIT) {
        break;
      }
    }

    // Emulate for 1/60 second
    cpu_interrupt(&cpu);
    while (cycles < 3333) {
      cpu_step(&cpu);
      cycles++;
    }
    cycles = 0;

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

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return EXIT_SUCCESS;
}
