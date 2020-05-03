#include <stdio.h>
#include <stdlib.h>

#include "debug.h"

u64 cpu_time_as_nanoseconds()
{
  struct timespec time;
  if (clock_gettime(CLOCK_MONOTONIC_RAW, &time) == -1) {
    perror("clock_gettime");
    exit(1);
  }
  u64 ns = time.tv_sec * 1000*1000*1000 + time.tv_nsec;
  return ns;
}

void init_debug_time(debug_time *dt, const char *name) {
  dt->start = cpu_time_as_nanoseconds();
  dt->name = name;
}

void end_debug_time(const debug_time *dt, const char *file, const char *function) {
  u64 end = cpu_time_as_nanoseconds();
  u64 delta = (end - dt->start);
  printf("%s:%s:%10s: %10luns\n", file, function, dt->name, delta);
}
