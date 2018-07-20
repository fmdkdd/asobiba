#include <stdio.h>

extern int entry_point() asm("entry_point");

const int BOOL_TAG   = 1;
const int BOOL_TRUE  = 0xffffffff;
const int BOOL_FALSE = 0x7fffffff;

int print(int r) {
  printf(" ");

  if (r & BOOL_TAG) {
    if (r == -1)              { printf("true");  }
    else if (r == 0x7FFFFFFF) { printf("false"); }
    else                      { fprintf(stderr, "Invalid value: 0x%x\n", r); }
  } else {
    // Number
    printf("%d", r >> 1);
  }

  return r;
}

int is_bool(int n) {
  if (n & BOOL_TAG) {
    return BOOL_TRUE;
  } else {
    return BOOL_FALSE;
  }
}

int is_num(int n) {
  if ((n & BOOL_TAG) == 0) {
    return BOOL_TRUE;
  } else {
    return BOOL_FALSE;
  }
}

int main() {
  int result = entry_point();
  print(result);
  return 0;
}
