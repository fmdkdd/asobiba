#ifndef DEBUG_H
#define DEBUG_H

#include <time.h>

#include "common.h"

typedef struct {
  u64 start;
  const char *name;
} debug_time;

u64 cpu_time_as_nanoseconds();
void init_debug_time(debug_time *dt, const char *name);
void end_debug_time(const debug_time *dt, const char *file, const char *function);

#ifdef BENCH
#define BEGIN_TIME(name)
#define END_TIME(name)
#else
#define BEGIN_TIME(name)                        \
  debug_time dt_##name;                         \
  init_debug_time(&dt_##name, #name);

#define END_TIME(name)                                  \
  end_debug_time(&dt_##name, __FILE__, __func__);
#endif

#endif
