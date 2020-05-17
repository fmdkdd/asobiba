#include "common.h"

void die(const char *const msg) {
  perror(msg);
  exit(1);
}
