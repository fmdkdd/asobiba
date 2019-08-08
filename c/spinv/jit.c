#define _POSIX_C_SOURCE 200809L

#include <stdlib.h>
#include <sys/mman.h>

#include "cpu.h"
#include "jit.h"

jit_function_cache jit_cache[100];

#define PAGE_SIZE 4096

// Compile block at ADDR and return a function to it.
static jit_function jit_compile(int addr, const CPU *cpu) {
  // Decode and translate until we jump

  const u8 *op = &cpu->ram[addr];

  u8 code[100];
  // fill with RET
  memset(code, 0xc3, sizeof(code));
  u8 *cp = code;

  // TODO: how to update PC/CPU state in compiled instructions?
  // TODO: count cycles

  bool done = false;
  while (!done) {
    switch (op[0]) {
    case 0x00: case 0x08: case 0x10: case 0x20:
      *cp++ = 0x90; // NOP
      break;

      // break on jumps
    case 0xc0: case 0xc1: case 0xc2: case 0xc3: case 0xc4: case 0xc5:
    case 0xc8: case 0xc9: case 0xca: case 0xcc: case 0xcd:
    case 0xd0: case 0xd2: case 0xd4: case 0xd7: case 0xd8:
    case 0xda: case 0xdc: case 0xe0: case 0xe2: case 0xe4:
    case 0xe8: case 0xea: case 0xec: case 0xf2: case 0xf4:
    case 0xf8: case 0xfa: case 0xfc:
      done = true;
      break;

    default:
      *cp++ = 0x90; // NOP
    }

    op++;
  }

 // malloc an aligned code page
 void *page;
 posix_memalign(&page, PAGE_SIZE, PAGE_SIZE);
 // fill with RET
 memset(page, 0xc3, PAGE_SIZE);
 // copy the code
 memcpy(page, code, sizeof(code));
 // turn the exec bit
 mprotect(page, PAGE_SIZE, PROT_READ | PROT_EXEC);
 // return a function pointer to it
 return (jit_function) page;
}

// Return the JITted block for ADDR, and compile it if necessary.
static jit_function jit_fetch(const CPU *cpu) {
  int pc = cpu->pc;
  jit_function_cache *f = &jit_cache[pc % 100];

  if (!f->valid || f->address != pc) {
    printf("recompiling function at 0x%04x\n", pc);
    f->function = jit_compile(pc, cpu);
    f->address = pc;
    f->valid = true;
  }

  return f->function;
}

// Look up in the JIT cache, and execute the result.
int jit_run(CPU *const cpu) {

  // TODO: instead of compiling all functions, maybe just the hot ones?

  return jit_fetch(cpu)(cpu);
}
