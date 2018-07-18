#include <stdio.h>

extern int entry_point() asm("entry_point");

const int BOOL_TAG = 1;

void print(int r) {
  if (r & BOOL_TAG) {
    if (r == -1)              { printf("true");  }
    else if (r == 0x7FFFFFFF) { printf("false"); }
    else                      { fprintf(stderr, "Invalid value: %x", r); }
  } else {
    // Number
    printf("%d", r >> 1);
  }

  printf("\n");
}

int main() {
  int result = entry_point();
  print(result);
  return 0;
}
