#include <stdio.h>

extern int entry_point() asm("entry_point");

int main() {
  int result = entry_point();
  printf("%d\n", result);
  return 0;
}
