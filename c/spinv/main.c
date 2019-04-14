#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "cpu.h"

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

void die(const char *const msg) {
  perror(msg);
  exit(1);
}

int main(int argc, char *argv[]) {

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

  while (true) {
    cpu_step(&cpu);
    cycles++;
  }

  return EXIT_SUCCESS;
}
