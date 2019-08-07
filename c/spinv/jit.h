#ifndef JIT_H
#define JIT_H

#include "cpu.h"

typedef int (*jit_function)(CPU *);

typedef struct {
  jit_function function;
  int address;
  bool valid;
} jit_function_cache;

int jit_run(CPU *const cpu);

#endif
