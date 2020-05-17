#ifndef JIT_H
#define JIT_H

#include "cpu.h"

typedef int (*jit_function)(CPU *);

typedef struct {
  jit_function function;
  int address;
  bool valid;
} JitFunctionCache;

typedef struct {
  CPU *cpu;
  int remaining_cycles;

  u32    exec_hits[0x10000];
  bool   hot_routines_marked[0x10000];
  u16    hot_routines[100];
  size_t hot_routines_size;

  JitFunctionCache function_cache[100];
} Jit;

void jit_init(Jit *jit, CPU *cpu);
void jit_dump_hot_routines(Jit *jit);
void jit_emulate_one_frame(Jit *jit);

#endif
