#ifndef DEBUG_H
#define DEBUG_H

#include <time.h>

#include "x86intrin.h"

typedef unsigned long long u128;
typedef unsigned long u64;
typedef double f64;

typedef struct {
  struct timespec start;
  const char *name;
} debug_time;

void init_debug_time(debug_time *dt, const char *name);
void end_debug_time(const debug_time *dt, const char *file, const char *function, int line);

#define BEGIN_TIME(name)                        \
  debug_time dt##name;                          \
  init_debug_time(&dt##name, #name);

#define END_TIME(name)                                          \
  end_debug_time(&dt##name, __FILE__, __func__, __LINE__);

#endif
