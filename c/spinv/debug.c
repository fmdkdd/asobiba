#include <stdio.h>

#include "debug.h"

void init_debug_time(debug_time *dt, const char *name) {
  clock_gettime(CLOCK_MONOTONIC, &dt->start);
  dt->name = name;
}

void end_debug_time(const debug_time *dt, const char *file, const char *function) {
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  u64 start_ns = dt->start.tv_sec * 1000*1000*1000 + dt->start.tv_nsec;
  u64 end_ns = end.tv_sec * 1000*1000*1000 + end.tv_nsec;
  u64 dt_ns = (end_ns - start_ns);
  printf("%s:%s:%s: %10luns\n", file, function, dt->name, dt_ns);
}
